/*
 *	search:
 *      =============================================================
 *	"header"		header del programa
 *	"ejemplos"		ejemplos de uso (lineas de comando )
 *	"documentacion" 	alguna documentacion
 *	"to do"			pendings ...
 *	"ojo"			cosas a tener en cuenta !!!!!
 *	"programa"		programa
 *
 *
 */

/*
 *	next ...
 *	ojo .. con un ! en colummna 10 ... no detecta que es comentario ...
 *	en cap_objt.f90 ... sigue sumando cantidad de allocate ..
 *
 *      ojo .. en grx_planning_routines ... 
 *	aparecio un caso con INTEGER*2 , y metio el allocate_vars en cualquier lugar 
 *
 *	atenti ...
 *	en la serie "busco_pri_d" ... etc
 *	hay que incluir ir tambien "implicit none" ... y va en la linea anterior
 *	(dr_booth ... )
 */


/*
 *	//header//
 *
 *	tfor.c
 *
 *	Sun Jun 16 19:20:43 -03 2024
 *	agregue que pueda cambiar inicializaciones tipo var/xxx/ por var = xxx
 *
 *	Mon Jun 10 01:30:37 -03 2024
 *	ajustes a armame_dos_lineas para separar lineas largas
 *
 *	Thu Jun  6 03:13:28 -03 2024
 *	mas mejoras en chgtyp
 *	agregue estadisticas para saber que cambios hizo
 *
 *	Mon Jun  3 19:23:14 -03 2024
 *	mejore que contemple integer *2, intent(int)
 *	
 *	
 *	Fri May 31 04:32:58 -03 2024
 *	agregue proc4 - lee todo el conjunto de fuentes, pero sobre cadenas de structs
 *	lo que necesito, es poder extender el largo de un archivo sin correr
 *	mas de 500,000 lines de codigo en un vector 
 *
 *	Mon May 27 22:07:23 -03 2024
 *	agregue sort a la lista de archivos de salida
 *
 *	Sun May 26 23:41:27 -03 2024
 *	mejore prue2 ... procesa todos los files en list_src, genera nueva
 *	version en new_repo, y tiene infraestructura armada para ...
 *	- sacar long names (l_names )
 *	- otras cosas que se quieran agregar a p_src2
 *
 *	Mon May 20 00:41:05 -03 2024
 *	agregue --chgmas .. saca los + de las declaraciones de variables
 *	con esto, se puede llevar un f77 a que compile como f95
 *	sin salir del fixed form
 *
 *
 *	Sat May 18 05:11:54 -03 2024
 *	en algunas lineas de fortran77, si primer car de lin es '*' no chilla !!
 *
 * 	Sat May 11 06:59:21 -03 2024
 *	agregue que funcione con logical real y character
 *
 *	Thu May  9 06:34:25 -03 2024
 *	agregue tool6 para arreglar lineas de continuacion
 *
 *	Sat May  4 04:43:47 -03 2024
 *	agregue a opcion "prue2" que genere los archivos cargados 
 *	en un nuevo directorio
 *
 *
 *	proyecto estabiliazcion de codigo fuente de fortran
 *
 * 	Opciones:
 * 	-h   		forma de uso
 *	-v		verbose
 *	-opciones=Xi	X = (D)ebug, (I)nformative, (E)xtra (user def)
 *
 *	para ponernos de acuerdo:
 *	0 - no muestra nada
 *	1 - solo informacion resultado que deba mostrarse
 *	2 - un poco de debug, 
 *          x que rutinas paso
 *          lineas cargadas 
 *          alguna info de control util
 *      3 - mas nivel de debug
 *
 *
 *	
 *
 *
 */



/*
 *	//ejemplos//
 *
 * 	Ejemplos de uso:
 *
 *	1) Mostrar uso
 *
 *	tfor
 *	tfor -h
 *
 *
 *	2) Opciones de debug
 *
 *	tfor -v                     activa debug, nivel full ... equivalente a d5 
 *	tfor -v -opciones=Ln        letra L (i informativo d debug e no_me_acuerdo)  
 *	                                  Nro n nivel 1 a 5 ... 5 full
 *	3) parser
 *
 *	parsea programa.for, output a copia.for en mismo formato, 
 *	si parser no reconoce caracter, lo pone en log2
 *	
 *	tfor -v -f -opciones=d5 -inp=programa.for -out=copia.for -aux=log2  
 *
 *	4) contar caracteres
 * 
 *	para comparar si programa1.for y p2.for tienen la misma cantidad de caracteres "utiles"
 *
 *	./tfor -v -opciones=d5 -inp=programa1.for -out=p1.chr -exec=1 > log1
 * 	./tfor -v -opciones=d5 -inp=p2.for        -out=p2.chr -exec=1 > log2
 *
 *	5) abre archivo, lee a vector en memoria para procesos varios y genera output
 *
 *	./prog  -v -f -opciones=d5 -proc=3 -src=input -out=output
 *
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	exec1
 *
 *	Toma archivo de input y genera tabla con cantidad de caraceres
 *	en todos el file
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	prue1:
 *
 *	Conjunto de 14 ejercicios sobre manejo de estructuras
 *
 *	./prog  -v -f -opciones=d5 -prue=1
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	prue2:
 *
 *	./tfor -v -opciones=d5 -prue=2 -inp=list_src -out=l_names -aux=p.err -dato=new_repo 
 *
 * 	Carga todos los archivos indicados en list_src, los carga en memoria
 * 	(como vector a punteros de estructuras)
 *	permite procesos varios, y vuelve a generar con cabios en el directorio new_repo
 *
 *	en p.err pone caracteres que no reconoce el parser de lineas
 *	en l_names (testing) pone nombres largos 
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool1: 
 *
 *	Carga dos lista l1 y l2, verifica que elementos de l2 estan / no estan en l1
 *
 *	./tfor -v -opciones=d5 -tool=1 -inp=l1 -in2=l2 -out=l_si -ou2=l_no
 *	./tfor -v -opciones=d5 -tool=1 -inp=l1 -in2=l2 -out=l_si -ou2=l_no -m
 *
 *	-m  el chequeo se hace en minusculas, pero la salida respeta el case original
 *	    sin -m el chequeo es case sensitive
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool2: 
 *
 *	Carga un archivo formato makefile y devuelve una lista de archivos de dependencias
 *
 *	./tfor -v -opciones=d5 -tool=2 -inp=makefile -out=lista
 *	./tfor -v -opciones=d5 -tool=2 -inp=makefile -out=lista -m   (genera en minusculas)
 *
 *
 *	carga file tipo makefile y saca lista de archivos involucrados
 *	arma lista unica (elimina dup) con archivos que tengan ext indicada
 *	por ahora: for, f90, f95, mon  (min y may )
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool3: 
 *
 *	Carga un archivo formato con nombre files y genera igual en minuscula
 *
 *	./tfor -v -opciones=d5 -tool=3 -inp=nom_file -out=nom_file_minusculas
 *
 *
 *	carga un file con listado de files y genera listado con mismos archivos,
 *	pasados a minuscula
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool4:
 *
 *	WIP
 *
 *	prototipo de cargar source en memo, trabajar, y grabarlo
 *	en este caso, se cargan las lineas en vector de punteros a estruct
 *	a dif de otros encuadres, que son descomp en tokens
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool5: 
 *
 *	Carga un archivo fuente, parsea, y deja listo para cambios
 *	pero ... parsea por linea !! no tiene visibilidad del resto del src
 *
 *
 *	./tfor -v -opciones=d5 -tool=5 -inp=a_org -out=a_new -f
 *
 *	./tfor -v -opciones=d5 -tool=5 -inp=a_org -out=a_new -f -nvd=1,2,3
 *
 *
 *	toma un fuente fortran a_org
 *	genera un nuevo fuente a_new
 *	-f genera file nuevo con lineas completas, sin descomp en tokens
 *	cambios :
 *	WIP 
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	tool6:
 *
 *	Carga source en memo, trabaja, y graba
 *	Carga como vector de punteros a estructuras lineas completa
 *	Parsea cada linea a token
 *	
 *	Arregla las lineas de comentarios
 *	el -t cambia tabs por 4 blancos
 *
 *	./tfor -v -opciones=d5 -tool=6 -t -inp=t1.for -out=t2.for --chgcomm
 *
 *	Arregla las lineas de continuacion en fortran
 *
 *	./tfor -v -opciones=d5 -tool=6 -inp=t1.for -out=t2.for --chglco > log
 *
 *
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - 
 *
 *	proc4:
 *
 *	Carga todos los sources en memo
 *	Utiliza cadenas de structs
 *	De esta manera, si tengo que extender un file por cambios
 *	no tengo que estar moviendo las mas de 500,000 lineas de codigo
 *
 *	./tfor -v -opciones=d5 -proc=4 -inp=l1 -out=l_names -aux=run.log -dato=repo2  > log
 *
 *
 */


/*
 *	//documentacion //
 */


/*
 * 	nivel de verbose
 *
 * 	-v	activa verbose (por defecto, se pone en 'd5' )
 * 	-v -opciones=XX
 *
 * 		XX puede ser d0 d1 d2 d3 d4 d5
 *
 *
 * 	d0	no imprime nada
 *
 * 	d1	muestra lo minimo indispensable, 
 * 		- comienzo y finalizacion de procesos
 * 		- resultados generales
 * 		- estadisticas
 *
 *	d2,d3	distintos niveles de debug / informacion
 *
 *	d4	imprime todo
 *	
 *	d5	especial para funciones gp
 *
 */



/*
 *	//to do //
 */


/*
 *	cfor_lcon da error con esta linea, pone el ampersand antes del !
 *	error....   CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',
 *
 *	--chgcom
 *	stadisticas 
 *
 *	--chgmas
 *	ej. ANDECOMP.FOR
 *
 *	poner contador cuando se aplanan las declaracions de vars
 *
 *	no funciona p4_mas cuando hay lineas con comentarios
 *	en medio de las lineas con mas 
 *
 *	Un archivo tipo .err ... fferr para guardar todos los 
 *	errores. Justo a cambios en error() .. que sea con 
 *	numero y un mensaje
 *
 *
 *	---- otros ----
 *
 *
 *	poner en orden los codigos de error !!!
 *
 *	errores
 *	manejar mejor, permitir imprimir algun string, 
 *      rutina en la que estaba etc
 *
 *	bulk process
 *	compatilizar las corridas individuales via 
 *	tool6, y las de tod el conjunto via prue2 
 *
 *	OJO !!!!
 *	si hay opcion -p ...
 *	poner -pro=algo .... trae problemas !!!
 *	no se puede usar la primera letras de los -v -p etc
 *
 */


/*
 *	errores
 *
 *	101	no pudo abrir archivo de input 
 *	102	no pudo abrir archivo de output 
 *	103	no 
 *	104	no 
 *	105	no 
 *
 *	111	no pudo abrir archivo de input src
 *	112	no pudo abrir archivo de input lst
 *
 *	501	overflow en busco_use
 *	502	overflow en busco_use
 *
 *	901	error en malloc
 *	902	error en malloc
 *	999	error debug
 */


/*
 *	gp_debug
 *
 *	1	todo
 *	3	tiene_multiple_vars	chequea si sente allac tiene mas de una variable 
 *	4	extraer_var_names	extra nombre de vars de sent allocate	
 *	5	agregar_stv		agrega stat=stv en sentencia allocate 
 *	6	hacer_lugar		corre todo el vector de lineas para hacer lugar 
 *	7	e_name			extrae extension de file - src
 *	8	tiene_use		chequea si hay un verdadero use algo en sent
 *	29	tipo-9
 */


/*
 *	//programa//
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <unistd.h>
#include <time.h>
#include <stdarg.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>


#define	DEBUG 0

#define	MAXD	16	/* buffer de juguete */
#define	MAXV	64	/* buffer chico */
#define	MAXR	128	/* buffer razonable */
#define	MAXB	1024	/* buffer grande */
#define	MAXT	256 	/* maximo de tokens en tabla */
#define	MAXF	64	/* largo de archivos de input - output - marcas etc */
#define	MAXP	128	/* maximo de palabras en general */
#define	HUG1	2048	/* huge buffer */
#define	HUG2	4096	/* huge buffer */
#define MSTR	48000	/* monster buffer */

#define TRUE 1
#define FALSE 0

/*
 *
 *	Funciones, variables y estructuras para manejo de parametros globales
 *
 */

#define	GP_SET 1
#define	GP_GET 0

int	gp_default();
int	gp_init(int ,char **);
int	gp_test();
int	gp_print();
int	gp_parser();
int	gp_fq(int,int);
int	gp_fverbose(char *);
char	*gp_tm();
char	*gp_fp(int,int,char **);

int	gp_q_partype1=0;	/* parametero tipo "name" */
int	gp_q_partype2=0;	/* parametero tipo "-something" */
int	gp_q_partype3=0;	/* parametero tipo "-someoption=somename" */
int	gp_q_partype4=0;	/* parametero tipo "--someoption" */


int	gp_verbose=0;		/* verbose 0 no 1 si */

int	gp_minusculas=0;	/* output en minuscula 0 no 1 si */
int	gp_tabs=0;		/* reemplazar tabs x 4 blancos si no */
int	gp_rellena=0;		/* & en cont de linea 0 ĺo pegam 1 rellena a columna 90 */
int	gp_fsentencia=0;	/* archivo de salida en formato sentencias 0 no 1 si */
int	gp_eol=0;		/* fuerzo string EOL al final de la linea ... a veces hay . en medio de la linea */
int	gp_reidx;		/* re indexar archivo de transacciones */
int	gp_pause;		/* pausa al mostrar ... */
int	gp_niveldes=0;		/* nivel de descripcion que se vuelca en archivo de salida (solo con fsentencia = 0 ) */
				/* 0 normal 1 sentencia y numero de token 2 .... agrego cosas del diccionario si se usa */
int	gp_cborrar=0;		/* comentarios a borrar 0 nada, 1 al final de lineas 2 lineas comentadas 3 todos     */
int	gp_suboption=0;		/* sub opciones para pro_xxxx algo  valores 1 2 3 ... etc  */

int	gp_help=0;		/* help 0 no 1 si */
int	gp_vers=0;		/* version 0 no 1 si */
int	gp_version(int);		/* version del fuente */
int	gp_uso(int);		/* usage */
				/* 0 normal 1 sentencia y numero de token 2 .... agrego cosas del diccionario si se usa */

int	gp_tpar[4];		/* vector con cantidad de parametros de cada tipo */

int	gp_proceed=0;		/* flag para anadir extra proceso dentro de un modulo general ... ej: exec9 .. ex9_p1 ... */
char	gp_proceed_st[MAXV];	/* contenido de proceed ... si es mas de un digito !! */
int	gp_debug=0;		/* flag extra para debug especifico */
char	gp_debug_st[MAXV];	/* contenido de proceed ... si es mas de un digito !! */

#if 0
char	*gp_e = " Entra a proceso ";
char	*gp_s = " Sale de proceso ";
#endif

char	*gp_e = " (+) ";
char	*gp_s = " (-) ";
char	*gp_m[2] = { " ((+)) ", " ((-)) " };
int	gp_cnt;
char	gp_p[MAXP];


char	gp_opciones[MAXV];	/* opciones adicionales, informadas de la forma -opciones=[letras]  */
char	gp_dato[MAXV];		/* dato adicional que se desee informar ... cualquier string */
char	gp_pruebas[MAXV];	/* pruebas ...    -prue=[nro]   ej:  -prue=1    */
char	gp_exec[MAXV];		/* execs ...      -exec=[rnp]   ej:  -exec=1    */
char	gp_proc[MAXV];		/* procs ...      -proc=[rnp]   ej:  -proc=1    */
char	gp_tool[MAXV];		/* procs ...      -tool=[rnp]   ej:  -tool=1    */
char	gp_util[MAXV];		/* util  ...      -util=[rnp]   ej:  -util=1    */
char	gp_fyh[MAXR];		/* fecha y hora */

char	gp_reportes[MAXV];	/* reportes a mostrar */

const char *fm1 = "%-20.20s";


/*
 *
 *	Funciones, variables y estructuras globales en el programa
 *
 */

int	abro_files();
int	cierro_files();
int	mostrar_cargas();
int	mostrar_reportes(char *);



int	gna(int,int);
char	*gnf(int);			/* genera fecha al azar */
char	*gnh(int,int);			/* genera hora al azar */
char	*gsf(char *);			/* string para imprimir fecha */
char	*gsh(char *);			/* string para imprimir hora */



char	*desde_igual(char *s);
char	*pasar_a_minusc(char *s);
char	*minusculas(char *s);
char	*df(char *);
int	clear_screen();


int	tiene_igual(char *);
int	linea_vacia(char *);
int	tipo_char(char );
int	es_numero(char);
int	es_num_tk(char *);
int	es_word(char *);
int	es_puntuacion(char *);
int	char_demed(char);
int	mostrar(int,int,char *);
int	error(int);

int	proceso_principal();
int	proc_principal();

int	pro_prue1();	/* ejercicios de estructuras, vectores, puntores, malloc */
int	pro_prue2();	/* carga todos los fuentes en memoria, en vector de ptr - version vieja */
int	pro_prue3();	/* ejercicio de acceso a una matriz, referenciada por un pointer */
int	pro_prue4();
int	pro_prue5();
int	pro_prue6();
int	pro_prue7();
int	pro_prue8();
int	pro_prue9();

int	pro_exec1();	/* cuenta caracteres en un archivo */
int	pro_exec2();	/* carga todos los fuentes en memoria, en vector de ptr - version vieja */
int	pro_exec3();	/* carga todos los fuentes en memoria, en vector de ptr - para procesos - version vieja */
int	pro_exec4();	/* carga todos los src en mem en vec de ptr para procesos - version nueva - entrys */
int	pro_exec5();	/* carga file con definicion de variables de fort con blancos inermedios y genera lista */
int	pro_exec6();	/* carga todos los src en mem en vec de ptr para procesos - version nueva - var con blanks */
int	pro_exec7();	/* pone bonita la salida de check1 y check2 ... */
int	pro_exec8();	/* carga todo el codebase en memo - busca variables fortran */
int	pro_exec9();	/* carga todo el codebase en memo - busca allocate */

int	pro_proc1();
int	pro_proc2();
int	pro_proc3();
int	pro_proc4();

int	pro_tool1();
int	pro_tool2();
int	pro_tool3();
int	pro_tool4();
int	pro_tool5();
int	pro_tool6();
int	pro_tool7();
int	pro_tool8();
int	pro_tool9();

int	pro_util1();
int	pro_util2();
int	pro_util3();
int	pro_util4();
int	pro_util5();
int	pro_util6();
int	pro_util7();
int	pro_util8();
int	pro_util9();




#define	TC_EOL	0
#define	TC_BLA	1
#define	TC_CCE	2	/* algunos caracteres especiales  ',;:/' */
#define	TC_PNT	3	/* punto '.' */
#define	TC_PAA	4	/* parent '([{' */
#define TC_PAC	5	/* parent ')]}' */
#define	TC_LET	6	/* letras */
#define	TC_NUM	7	/* numero */
#define	TC_CVR	8	/* caracteres varios ' % */
#define	TC_GBJ	9	/* guion bajo '_' */
#define	TC_CHO	10	/* 0 - choto */
#define	TC_RST	99

char	finp[MAXF];	/* archivo inp para entrada segun necesidad */
char	fin2[MAXF];	/* archivo inp para entrada segun necesidad */
char	fin3[MAXF];	/* archivo inp para entrada segun necesidad */
char	fout[MAXF];
char	fou2[MAXF];
char	fou3[MAXF];
char	fou4[MAXF];
char	fou5[MAXF];
char	fou6[MAXF];
char	fou7[MAXF];
char	faux[MAXF];	/* archivo aux para salidas segun necesidad */
char	flog[MAXF];	/* archivo log para salidas segun necesidad */
char	fsta[MAXF];	/* archivo stats segun necesidad */
char	fckf[MAXF];	/* archivo de mensajes de check segun necesidad */
char	fdbg[MAXF];	/* archivo de pautas de debug */

char	fsrc[MAXF];	/* archivo de src de entrada */
char	flst[MAXF];	/* archivo de listado de src de entrada */

int	ffinp;
int	ffin2;
int	ffin3;
int	ffout;
int	ffou2;
int	ffou3;
int	ffou4;
int	ffou5;
int	ffou6;
int	ffou7;
int	ffaux;		/* archivo aux para output segun necesidad */
int	fflog;		/* archivo log para output segun necesidad */
int	ffcfg;		/* archivo de configuracion */
int	ffsta;		/* estadisticas */
int	ffckf;		/* check file messages */
int	ffdbg;		/* debug   */

int	ffsrc;
int	fflst;

int	ffprb;		/* utilizo -prueba=1 ... 2 .. 3 etc */
int	ffexc;		/* utilizo -exec=1 .. 2  .. 3 etc   */
int	ffpro;		/* utilizo -proceso=1 .. 2 .. 3 etc */
int	fftoo;		/* utilizo -tool=1 .. 2 .. 3 etc */
int	ffuti;		/* utilizo -tool=1 .. 2 .. 3 etc */

int	ffdat;		/* utilizo -dato */

int	fftb1;
int	fftb2;
int	fftb3;
int	fftb4;

FILE	*hfinp;
FILE	*hfin2;
FILE	*hfin3;
FILE	*hfout;
FILE	*hfou2;
FILE	*hfou3;
FILE	*hfou4;
FILE	*hfou5;
FILE	*hfou6;
FILE	*hfou7;
FILE	*hfaux;
FILE	*hflog;
FILE	*hfsta;
FILE	*hfckf;
FILE	*hfdbg;

FILE	*hfsrc;
FILE	*hflst;

FILE	**fptr;


int	flag_caracteres;
int	flag_before_exit;
int	flag_alloc_ok;

int	agregar_ffaux(char *);
int	agregar_ffsta(char *);
int	agregar_ffckf(char *);
int	agregar_ffdbg(char *);
int	agregar_fflog(char *);


/*
 * 	algunos settings especificos de tfor - conv de for files 
 *
 */

int	ffchg_dec  = 0;		/* delete comentarios            */
int	ffchg_com  = 0;		/* cambia caracter de comentario */
int	ffchg_typ  = 0;		/* cambia type selectors */
int	ffchg_mas  = 0;		/* saca la continuacion de linea con mas */
int	ffchg_ini  = 0;		/* cambia el tipo de init de vars        */
int	ffchg_lco  = 0;		/* cambia lineas de continuacion */
int	ffchg_vcb  = 0;		/* cambia variables con blancos intermedios */
int	ffchg_alo  = 0;		/* cambia allocate , agrega check  */



/*
 *	Estructuras y variables para la lista de variables fortran con blancos
 *
 *
 */

#define	MAX_VCB		600	/* maxima cantidad de variables con blancos en codebase */
#define	MAX_VSB		600	/* maxima cantidad de variables con blancos en codebase */

int	q_vcb;
char	vcb[MAX_VCB][MAXV];
char	vcb_o[MAX_VCB][MAXV];
char	vcb_p[MAX_VCB][MAXB];
char	vcb_c[MAX_VCB][MAXB];
char	vcb_d[MAX_VCB][MAXB];

int	q_vsb;
char	vsb[MAX_VSB][MAXV];



/*
 *	Estructuras y variables para tokens de las lineas procesadas
 *	en este caso, estan defindas localmente en proc parser
 *	de ser necesario acceder globalmente, definir aca
 *
 */


/* lo necesito para l_pars */
#if 1

int	q_tk;
char	tk[MAXT][MAXB];

#endif


/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *	const y estructs especificas para todo el tema de estabilizacion de fortran
 *	(version cadena de structs )
 *
 *	ldf	linea de file
 */

#define	MAX_QSRC	500   		/* cantidad de files ... 500  */

typedef	struct	ldf	*ldf_ptr;
typedef	struct	ldf
{	char	l[HUG1];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
	struct	ldf	*nx;
}	ldf_node;

ldf_ptr	ldfp1,ldfp2,*ldfq1,*ldfq2;

ldf_ptr	vldf[MAX_QSRC];			/* vector a punteros primer struct de c fuente */




/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *	const y estructs especificas para todo el tema de estabilizacion de fortran
 *	(version vector de punteros a structs )
 */


#define	MAX_FSRC	400000		/* 10000 lineas de codigo ... seran suf ? */
int	qf_lin;				/* lineas en file */
int	qf_fen;				/* strings con nombre de archivos validos encontrados */
int	qf_src;				/* cantidad total de lineas de src en vector */

typedef	struct	tfn	*fnptr;
typedef	struct	tfn
{	char	l[HUG2];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	node;

fnptr	fnp1,fnp2,*fnq1,*fnq2,*fnpa;

fnptr	fnp[MAX_FSRC];			/* vector de punteros a lineas de source */
fnptr	fnf[MAX_FSRC];			/* vector de punteros a lineas de archivos encontrados */

int	pf_load();			/* proceso de carga del file */
int	qfv_load(FILE *,fnptr *,int *);	/* proceso de carga del file - como vector de ptrs a struct */
int	qf_addline(fnptr *,int );	/* agrega una linea */
int	qf_move(fnptr *, int);		/* hace un  'lugar' en el vector */
int	pf_write();			/* proceso de write del file */

int	qfc_load(FILE *,int,int *);	/* proceso de carga del file - como cadena de ptrs a struct */
int	qfc_write(FILE *,int);


/*
 * 	MAX_QSRC		500		
 */

int	qf_ff;				/* cantidad de archivos cargados en memo */

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf,ql;		/* primera - ultima fila, q lineas */
	int	f1,f2,f3;		/* flags prop general */
	int	q_entrys;		/* cantidad de entrys */
	int	q_prog_calling;		/* cant de programas (dif) que llaman a entrys */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];


/*
 *
 *	otras rutinas especificas de buscadores en source fortran 
 *
 */

int	busco_use();
int	encuentro_use(char *, char *);
int	is_use_valid(char);
int	linea_comentario(char *);




/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *
 *	Estructura de nodoslinkeados al siguiente
 *
 */



int	load_file(FILE *);

int	q_wrd;				/* cantidad de palabras en diccionario */

typedef	struct	pnode	*pnodeptr;
typedef	struct	pnode
{	char	*wrd;
	int	num;
	struct	pnode	*nx;
}	knode;

pnodeptr	npx,np1,*npa;





int	load_makefile(FILE *);
int	load_makefile2(FILE *);
int	proc_makefile();
int	write_makefile(FILE *);
int	filter_makefile();
int	to_min();
int	es_char_v(char);
int	es_str_v(char *);

int	check_c1();
int	write_result();
int	check_fnames();
char	*extract_fname(char *);

int	linea_vacia_for(char *);
int	cfor_comm(int *,int *);
int	cfor_vars(int *,int *);
int	cfor_lcon(int *,int *);
int	cfor_mas(int *,int *);
int	cfor_ini(int *,int *);
int	cfor_vcb(int *,int *);
int	cfor_dec(int *,int *);
int	cfor_alo(int *,int *);
int	check_file(int *);
int	l_pars(int, int *);
int	tiene_dec_var1();
int	tiene_mas(char *);
int	tiene_amper(char *);
int	def_var_continua(char *s);
int	fix_dec_var1();
int	fix_dec_var2();
int	p_src1();
int	p_src2();
int	p_src3();
int	ex3_p1();
int	ex3_p2();
int	ex3_p3();
int	ex3_p4();
int	ex4_p1();
int	ex6_p1();
int	ex6_p2();
int	ex6_p3();
int	ex6_p4();
int	ex8_p1();
int	ex8_p2();
int	ex8_p3();
int	ex9_p1_a();
int	ex9_p1_b();
int	ex9_p1_c();
int	ex9_p2();
int	ex9_p3();
int	es_cadena_valida(int,char *);
int	es_cadena_interesante(char *);
int	es_cadena_int_src3(char *,int *);
int	tiene_coment_intermedio (char *);
int	es_linea_comentario(char *);
int	es_linea_comentario2(char *);
int	armame_dos_lineas(char *, char *, char *);
int	preparame_dos_lineas(char *, char *, char *);
int	correme_una_linea(int, int);
int	pasa_filtro(char *);
int	ordenar_makefile();
int	ps_src1();
int	largo_linea(char *);
int	tiene_include_valido(char *,char *,char *);
int	tiene_include_v2(char *, char *);
int	es_nombre_de_include(char *s);
int	lne(int );
char	*trim_blanks(char *);
char	*trim_blanks_beg(char *);
char	*limpiar_mas(char *);
char	*analisis_comentario(char *);
bool	tiene_include_v3(char *, char *);
bool	tiene_include_v4(char *, char *);
int	tiene_entry_v1(char *, char *);
int	tiene_call_a_entry(char *, char *);
int	tiene_function_a_entry(char *, char *);
int	tiene_coment_final(char *, int*);
int 	tiene_entry(const char *); 
int	complejidad_mon(int, int, int *, int *);
int	es_mon(char *);
int	len_linea_src(char *,int *,int *,int *);
int	lml_src_sc(int *,int *,int , int );
int	primer_char(char *, int *);
char	*f_name(int);					/* devuelve nombre de src en funcion de nro de fila en fnp */
char	*e_name(int);					/* devuelve extension de src en funcion de nro de fila en fnp */
char	*b_name(int);
int	n_name(int);					/* devuelve subindice en tb, en funcion de nro de fila en fnp */
int	tiene_integer(char *);
int	tiene_character(char *);
int	rutina_v(char *, int *, char **);
int	rutina_w(char *, int *, int *,char **);
int	compare_vcb(const void *, const void *);
int	primer_caracter_es_mas(char *);
int	tiene_allocate(char *);
char	*trim_beg(char *);
char	*trim_beg_f90(char *);
char	*trim_end(char *);
char	*trim_end_f90(char *);
char	*extract_var_name(char *);
char	*chanchada(char *);
char	*chanchada_for(char *);
int	cargar_debug_def(int *);
int	cantidad_de_vars(char *, int *);
int	hacer_lugar(int,int,int,int,int);
int	hacer_lugar_itz(int,int,int,int,int);
int	hacer_lugar_mf(int,int);
char	*quitar_blancos_molestos(char *);



int	es_integer2(char *s);		/* ojo. especificas de pro_exec5 */
int	es_character1(char *s);


#if 0
int	info_cmp1(char *, char *);
int	info_cmp2(char *, char *);
#endif

int 	info_cmp1(const void *, const void *);
int 	info_cmp2(const void *, const void *);

#if 0
void v_pars(char *, int *, int *, char **, int *, DictionaryEntry *, int );
#endif

void v_pars(char *, int *, int *, char **);

#define	MAX_VARS	200

int	n_var;
int	v_size[MAX_VARS];
char	*v_name[MAX_VARS];

int	borrar_stats();


/*
 *	variables para estadisticas en los procesos 
 *	estandarizacion del bendito fortran
 *	stats
 */


int	sq_lineas_com;			/* q de lineas de comentarios cambiadas */
int	sq_lineas_desdobladas;		/* q de lineas que hubo que dividir */
int	sq_integer;			/* q de integer cambiados */
int	sq_real;			/* q de real cambiados */
int	sq_logical;			/* q de logical cambiados */
int	sq_character;			/* q de character cambiados */
int	sq_variables_no_convertidas;	/* importante ! variables que no se pud convertir */
int	sq_lcont;			/* lineas de continuacion modificadas */
int	sq_vinit_simple;		/* var inits cambiados - simple ...en una linea */
int	sq_lineas_con_mas_elim;		/* lineas con cont de linea mas eliminadas */
int	sq_comentarios_elim;   		/* lineas de codigo comentadas  eliminadas */
int	sq_coment_al_final_elim;	/* comentarios al final de lineas de codigo validas eliminadas */
int	sq_q_alloc;			/* cantidad total de allocs */

int	sq_q_alloc_for_sta;		/* cantidad total de lineas allocate con stat ya incorporado */
int	sq_q_alloc_for_slc;		/* cantidad total de allocs en f77 sin lineas de continuacion */
int	sq_q_alloc_for_clc;		/* cantidad total de allocs en f77 con lineas de continuacion */
int	sq_q_alloc_for_sav;		/* cantidad total de lineas allocate con single var */
int	sq_q_alloc_for_mav;		/* cantidad total de lineas allocate con multiple var */
int	sq_q_alloc_for_slc_sav;		/* cant tot de lineas sin cont con allocate single var */
int	sq_q_alloc_for_clc_sav;		/* cant tot de lineas con cont con allocate single var */
int	sq_q_alloc_for_slc_mav;		/* cant tot de lineas sin cont con allocate multi var */
int	sq_q_alloc_for_clc_mav;		/* cant tot de lineas con cont con allocate multi var */

int	sq_q_alloc_f90_sta;		/* cantidad total de lineas allocate con stat ya incorporado  */
int	sq_q_alloc_f90_slc;		/* cantidad total de allocs en f90 sin lineas de continuacion */
int	sq_q_alloc_f90_clc;		/* cantidad total de allocs en f90 con lineas de continuacion */
int	sq_q_alloc_f90_sav;		/* cantidad total de lineas allocate con single var */
int	sq_q_alloc_f90_mav;		/* cantidad total de lineas allocate con multiple var */
int	sq_q_alloc_f90_slc_sav;		/* cant tot de lineas sin cont con allocate single var */
int	sq_q_alloc_f90_clc_sav;		/* cant tot de lineas con cont con allocate single var */
int	sq_q_alloc_f90_slc_mav;		/* cant tot de lineas sin cont con allocate multi var */
int	sq_q_alloc_f90_clc_mav;		/* cant tot de lineas con cont con allocate multi var */

int	sg_q_alloc_t01;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t02;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t03;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t04;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t05;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t06;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t07;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t08;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t09;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t10;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t11;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t12;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t13;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t14;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t15;			/* cant tot de allocs tipo ... */
int	sg_q_alloc_t16;			/* cant tot de allocs tipo ... */

/*
 *	prueba loca
 *
 */



int	st_load();			/* funcion de carga de listas */
int	q_st;                           /* cantidad de palabras en lista */

typedef	struct	tst	*stptr;
typedef	struct	tst
{	char	*wrd;
	int	num;
	struct	tst	*nx;
}	nlst;

stptr	st_p1,st_p2,*st_n1,*st_n2,**st_nx,**st_ny;



#if 0
Definicion de chatGPT de esta struct:


typedef struct  tnode   *nodeptr;
typedef struct  tnode
{       char    *wrd;
	int     num;
	struct  tnode   *nx;
}       node;

nodeptr npx,np1,*npa;


Vamos a analizar cada línea de código C y explicar qué es cada cosa:

1. `typedef struct tnode *nodeptr;`
   
   Esta línea declara un alias (`nodeptr`) para un puntero a una estructura `tnode`. 
   En otras palabras, `nodeptr` es un tipo de dato que representa un puntero a un `tnode`. 
   Este tipo de dato se utilizará para manejar punteros a nodos en una lista enlazada.

2. `typedef struct tnode`
   `{ char *wrd;`
   `  int num;`
   `  struct tnode *nx;`
   `} node;`

   Esta es la definición de una estructura (`struct`) llamada `tnode`. 
   La estructura `tnode` tiene tres miembros:
   - `char *wrd;`: Un puntero a un carácter (`char`), que representa una cadena de caracteres (string).
   - `int num;`: Un entero (`int`), que puede utilizarse para almacenar un número.
   - `struct tnode *nx;`: Un puntero a otro `tnode`, es decir, un puntero a otra 
      estructura del mismo tipo (`struct tnode`). 

   Esto permite crear una lista enlazada de nodos.

   La definición también crea un alias (`node`) para la estructura `tnode`, 
   por lo que la estructura puede referirse como `node` en lugar de `struct tnode`.

3. `nodeptr npx, np1, *npa;`

   Esta línea declara tres variables:
   - `npx` y `np1`: Ambas son variables de tipo `nodeptr`, es decir, punteros a `node` o `tnode`.
   - `*npa`: Esta es una variable que apunta a un puntero (`nodeptr`). 
   En otras palabras, `npa` es un puntero a un puntero a `node` o `tnode`.

En resumen, este código define una estructura `node` con 
un puntero a una cadena de caracteres (`wrd`), 
un entero (`num`) y un puntero a otro nodo (`nx`), 
y crea alias para manejar punteros a la estructura. 
Además, declara variables de tipos `nodeptr` y un puntero a `nodeptr`.



#endif





#define	LF77	72			/* maximo de caracteres en una lunia de F77 */


char	*argum[3] = { "tfor","-v","-opciones=d5" };

char	**mi_argv = &argum[0];
int	mi_argc = 3;


char	*mis_args[100];
int	mis_argc;
char	**mis_argv = &mis_args[0];



/*	vector de punteros a variables en allocate multi var 
 *	200 deberia alcanzar !!!
 */

#define MAX_VARS 200		/* maxima cantidad de modulos a cargar para debug */
#define MAX_LEN 1024

char	*gv_str[MAX_VARS][MAXB];

/*	debug stuff
 *
 */

#define	MAX_DBG	100
int	q_dbg;
char	tdbg[MAX_DBG][MAXV];
int	tflg[MAX_DBG];

/*
 * me alcanza para hacer un diccionario con los comentarios ?
 *
 */


char	cpt[18000][MAXB];

	
/*
 *	vector para cargar nombre de files 
 *	para agregar allocate_vars.obj
 *
 *
 */


#define	MAX_ALLOC_FILES	150

char	tb_x1[MAX_ALLOC_FILES][MAXR];

/*
 *	especial para tema makefile
 *
 */



#define	MAX_FSRCM	2000		/* 2000 lineas de codigo de makefile ... seran suf ? */
#define	MAXM		6000		/* linea mas larga en makefile */

typedef	struct	tfnm	*fnptrm;
typedef	struct	tfnm
{	char	l[MAXM];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	nodem;

fnptrm	fnp1m,fnp2m,*fnq1m,*fnq2m,*fnpam;

fnptrm	fnpm[MAX_FSRCM];		/* vector de punteros a lineas de source */

/*
 * -----------------------------------------------------------------------------------
 *
 *	main
 *
 * -----------------------------------------------------------------------------------
 */

int	main(argc,argv)
int	argc;
char	**argv;
{
	int	n;

	/* miscelaneas gobales */
	flag_before_exit = 0;
	srand (time(NULL));

	/* init varios */
	gp_default();
	gp_init(argc,argv);

	gp_parser();
	gp_version(gp_vers);

	char	z[MAXV];
	sprintf (z,"proceso main");


	/* shit finger ? */
	if (gp_help)
		gp_uso(0);


	/* programa */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
		gp_print();
	}


	/* opciones ingresadas  */
	if (gp_fverbose("d2"))
	{	gp_test();
	}


	/* abrir files */
	abro_files();

	/* proceso principal */
	proceso_principal();

	/* cerrar files */
	cierro_files();



	/* reportes ... */
	if (gp_fverbose("i1"))
	{
		mostrar_reportes("default");
	}


	/* si hay algo que hacer antes de salir, es aca ! */
	if (flag_before_exit)
	{	
		printf ("Atencion: .... \n");
	}

	/* programa */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	reportes 
 *
 * 	muestra reportes al finalizar todos los procesos
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * stats
 *
 * imprime / guarda estadisticas 
 *
 */


int	mostrar_reportes(s)
char	*s;
{
	int	f1;
	int	n;

	char	z[MAXV];
	sprintf (z,"mostrar_reportes");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	n = 0;
	if (!strcmp(s,"default"))
		n = 1;
	if (!strcmp(s,"tool6"))
		n = 10;
	if (!strcmp(s,"exec9"))
		n = 11;



	if (n == 1)
	{
		printf ("hola Erno ...               \n");
		printf ("que gusto verte por aca ... \n");
	}


	if (n == 10 )
	{

		printf ("\n");
		printf ("File: %s\n",finp);
		printf ("Cantidad de lineas con coment camb  %6d \n", sq_lineas_com);
		printf ("Cantidad de lineas con com elim     %6d \n", sq_comentarios_elim);
		printf ("Cantidad de lineas divididas        %6d \n", sq_lineas_desdobladas);
		printf ("Cantidad de integer cambiados       %6d \n", sq_integer);
		printf ("Cantidad de real cambiados          %6d \n", sq_real);
		printf ("Cantidad de logical cambiados       %6d \n", sq_logical);
		printf ("Cantidad de character cambiados     %6d \n", sq_character);
		printf ("Cantidad de lin cont cambiadas      %6d \n", sq_lcont);
		printf ("Variables_no_convertidas            %6d \n", sq_variables_no_convertidas);
		printf ("Var inits simples convertidos       %6d \n", sq_vinit_simple);
		printf ("Lineas con cont mas convertidas     %6d \n", sq_lineas_con_mas_elim);
		printf ("Comentarios al final de lineas elim %6d \n", sq_coment_al_final_elim);	
		printf ("Cant de alloc totales               %6d \n", sq_q_alloc);
		printf ("Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		printf ("Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		printf ("Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		printf ("Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		printf ("Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		printf ("Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */

		if (ffsta)
		{

		fprintf (hfsta,"%s","\n");
		fprintf (hfsta,"File: %s\n",finp);
		fprintf (hfsta,"Cantidad de lineas con coment camb  %6d \n", sq_lineas_com);
		fprintf (hfsta,"Cantidad de lineas con com elim     %6d \n", sq_comentarios_elim);
		fprintf (hfsta,"Cantidad de lineas divididas        %6d \n", sq_lineas_desdobladas);
		fprintf (hfsta,"Cantidad de integer cambiados       %6d \n", sq_integer);
		fprintf (hfsta,"Cantidad de real cambiados          %6d \n", sq_real);
		fprintf (hfsta,"Cantidad de logical cambiados       %6d \n", sq_logical);
		fprintf (hfsta,"Cantidad de character cambiados     %6d \n", sq_character);
		fprintf (hfsta,"Cantidad de lin cont cambiadas      %6d \n", sq_lcont);
		fprintf (hfsta,"Variables_no_convertidas            %6d \n", sq_variables_no_convertidas);
		fprintf (hfsta,"Var inits simples convertidos       %6d \n", sq_vinit_simple);
		fprintf (hfsta,"Lineas con cont mas convertidas     %6d \n", sq_lineas_con_mas_elim);
		fprintf (hfsta,"Comentarios al final de lineas elim %6d \n", sq_coment_al_final_elim);	
		fprintf (hfsta,"Cant de alloc totales               %6d \n", sq_q_alloc);
		fprintf (hfsta,"Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		fprintf (hfsta,"Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		fprintf (hfsta,"Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		fprintf (hfsta,"Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		fprintf (hfsta,"Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		fprintf (hfsta,"Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */

		}
	}






	if (n == 11 )
	{
		printf ("Cant de alloc TIPO-01   for sl sv no stat    %6d \n", sg_q_alloc_t01);
		printf ("Cant de alloc TIPO-02   for sl sv w/ stat    %6d \n", sg_q_alloc_t02);
		printf ("Cant de alloc TIPO-03   for sl mv no stat    %6d \n", sg_q_alloc_t03);
		printf ("Cant de alloc TIPO-04   for sl mv w/ stat    %6d \n", sg_q_alloc_t04);
		printf ("Cant de alloc TIPO-05   for ml sv no stat    %6d \n", sg_q_alloc_t05);
		printf ("Cant de alloc TIPO-06   for ml sv w/ stat    %6d \n", sg_q_alloc_t06);
		printf ("Cant de alloc TIPO-07   for ml mv no stat    %6d \n", sg_q_alloc_t07);
		printf ("Cant de alloc TIPO-08   for ml mv w/ stat    %6d \n", sg_q_alloc_t08);

		printf ("   \n");

		printf ("Cant de alloc TIPO-09   f90 sl sv no stat    %6d \n", sg_q_alloc_t09);
		printf ("Cant de alloc TIPO-10   f90 sl sv w/ stat    %6d \n", sg_q_alloc_t10);
		printf ("Cant de alloc TIPO-11   f90 sl mv no stat    %6d \n", sg_q_alloc_t11);
		printf ("Cant de alloc TIPO-12   f90 sl mv w/ stat    %6d \n", sg_q_alloc_t12);
		printf ("Cant de alloc TIPO-13   f90 ml sv no stat    %6d \n", sg_q_alloc_t13);
		printf ("Cant de alloc TIPO-14   f90 ml sv w/ stat    %6d \n", sg_q_alloc_t14);
		printf ("Cant de alloc TIPO-15   f90 ml mv no stat    %6d \n", sg_q_alloc_t15);
		printf ("Cant de alloc TIPO-16   f90 ml mv w/ stat    %6d \n", sg_q_alloc_t16);

		printf ("   \n");

#if 0
		if (ffsta)
		{
		fprintf (hfsta,"Cant de alloc totales               %6d \n", sq_q_alloc);
		fprintf (hfsta,"Cant de alloc totales for con stat  %6d \n", sq_q_alloc_for_sta);		/* for con stat ya hecho */
		fprintf (hfsta,"Cant de alloc totales f90 con stat  %6d \n", sq_q_alloc_f90_sta);		/* for con stat ya hecho */

		fprintf (hfsta,"Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		fprintf (hfsta,"Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		fprintf (hfsta,"Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		fprintf (hfsta,"Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */
		fprintf (hfsta,"Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_for_slc_sav);
		fprintf (hfsta,"Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_for_clc_sav);
		fprintf (hfsta,"Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_for_slc_mav);
		fprintf (hfsta,"Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_for_clc_mav);

		fprintf (hfsta,"Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		fprintf (hfsta,"Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		fprintf (hfsta,"Cant de alloc totales f90 sav       %6d \n", sq_q_alloc_f90_sav);
		fprintf (hfsta,"Cant de alloc totales f90 mav       %6d \n", sq_q_alloc_f90_mav);
		fprintf (hfsta,"Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_f90_slc_sav);
		fprintf (hfsta,"Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_f90_clc_sav);
		fprintf (hfsta,"Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_f90_slc_mav);
		fprintf (hfsta,"Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_f90_clc_mav);

		}
#endif
	}

















	if (n == 13 )
	{
		printf ("Cant de alloc totales               %6d \n", sq_q_alloc);
		printf ("Cant de alloc totales for con stat  %6d \n", sq_q_alloc_for_sta);		/* for con stat ya hecho */
		printf ("Cant de alloc totales for f90 stat  %6d \n", sq_q_alloc_f90_sta);		/* for con stat ya hecho */

		printf ("                                        \n");
		printf ("Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		printf ("Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		printf ("Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		printf ("Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */
		printf ("Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_for_slc_sav);
		printf ("Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_for_clc_sav);
		printf ("Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_for_slc_mav);
		printf ("Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_for_clc_mav);

		printf ("                                        \n");
		printf ("Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		printf ("Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		printf ("Cant de alloc totales f90 sav       %6d \n", sq_q_alloc_f90_sav);
		printf ("Cant de alloc totales f90 mav       %6d \n", sq_q_alloc_f90_mav);
		printf ("Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_f90_slc_sav);
		printf ("Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_f90_clc_sav);
		printf ("Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_f90_slc_mav);
		printf ("Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_f90_clc_mav);
 


		if (ffsta)
		{
		fprintf (hfsta,"Cant de alloc totales               %6d \n", sq_q_alloc);
		fprintf (hfsta,"Cant de alloc totales for con stat  %6d \n", sq_q_alloc_for_sta);		/* for con stat ya hecho */
		fprintf (hfsta,"Cant de alloc totales f90 con stat  %6d \n", sq_q_alloc_f90_sta);		/* for con stat ya hecho */

		fprintf (hfsta,"Cant de alloc totales for slc       %6d \n", sq_q_alloc_for_slc);
		fprintf (hfsta,"Cant de alloc totales for clc       %6d \n", sq_q_alloc_for_clc);
		fprintf (hfsta,"Cant de alloc totales for sav       %6d \n", sq_q_alloc_for_sav);		/* single var allocated */
		fprintf (hfsta,"Cant de alloc totales for mav       %6d \n", sq_q_alloc_for_mav);		/* multip var allocated */
		fprintf (hfsta,"Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_for_slc_sav);
		fprintf (hfsta,"Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_for_clc_sav);
		fprintf (hfsta,"Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_for_slc_mav);
		fprintf (hfsta,"Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_for_clc_mav);

		fprintf (hfsta,"Cant de alloc totales f90 slc       %6d \n", sq_q_alloc_f90_slc);
		fprintf (hfsta,"Cant de alloc totales f90 clc       %6d \n", sq_q_alloc_f90_clc);
		fprintf (hfsta,"Cant de alloc totales f90 sav       %6d \n", sq_q_alloc_f90_sav);
		fprintf (hfsta,"Cant de alloc totales f90 mav       %6d \n", sq_q_alloc_f90_mav);
		fprintf (hfsta,"Cant de alloc tot sin cont sing var %6d \n", sq_q_alloc_f90_slc_sav);
		fprintf (hfsta,"Cant de alloc tot con cont sing var %6d \n", sq_q_alloc_f90_clc_sav);
		fprintf (hfsta,"Cant de alloc tot sin cont mult var %6d \n", sq_q_alloc_f90_slc_mav);
		fprintf (hfsta,"Cant de alloc tot con cont mult var %6d \n", sq_q_alloc_f90_clc_mav);

		}
	}









	if (n == 0)
	{
		printf ("hola Erno ...               \n");
		printf ("que tengas un buen dia  ... \n");
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}



}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	reportes 
 *
 * 	init de variables
 *
 * -----------------------------------------------------------------------------------
 */


int	borrar_stats()
{

	sq_lineas_com            = 0;
	sq_comentarios_elim      = 0;
	sq_lineas_desdobladas    = 0;
	sq_integer               = 0;	
	sq_real	                 = 0;	
	sq_logical	         = 0;
	sq_character             = 0;
	sq_lcont                 = 0;
	sq_vinit_simple          = 0;
	sq_lineas_con_mas_elim   = 0;
	sq_coment_al_final_elim  = 0;	

}





/*
 * -----------------------------------------------------------------------------------
 *
 * 	proceso_principal
 *
 * 	procesa zonas definidas en el mapa
 *
 * -----------------------------------------------------------------------------------
 */

int	proceso_principal()
{

	char	z[MAXV];
	sprintf (z,"proceso principal");

	int	i;
	int	px,py;

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


#if 0
	/* muestro resultado de cargas, si las hubo */
	if (gp_fverbose("d3"))
	{
		mostrar_cargas();
	}
#endif



	/* borro estadisticas */
	borrar_stats();

	/* habilito debug */
	if (ffdbg)
		cargar_debug_def(&q_dbg);

	/* proceso por defecto si no selecciono otro tipo de proc */
	if (!ffprb && !ffexc && !ffpro && !fftoo && !ffuti )
		proc_principal();


	if ( ffprb)
	{
		if (ffprb == 1)
			pro_prue1();
		if (ffprb == 2)
			pro_prue2();
		if (ffprb == 3)
			pro_prue3();
		if (ffprb == 4)
			pro_prue4();
		if (ffprb == 5)
			pro_prue5();
		if (ffprb == 6)
			pro_prue6();
		if (ffprb == 7)
			pro_prue7();
		if (ffprb == 8)
			pro_prue8();
		if (ffprb == 9)
			pro_prue9();
	}	

	if ( ffexc)
	{
		if (ffexc == 1)	
			pro_exec1();
		if (ffexc == 2)
			pro_exec2();
		if (ffexc == 3)
			pro_exec3();
		if (ffexc == 4)
			pro_exec4();
		if (ffexc == 5)
			pro_exec5();
		if (ffexc == 6)
			pro_exec6();
		if (ffexc == 7)
			pro_exec7();
		if (ffexc == 8)
			pro_exec8();
		if (ffexc == 9)
			pro_exec9();
	}

	if ( ffpro)
	{
		if (ffpro == 1)	
			pro_proc1();
		if (ffpro == 2)
			pro_proc2();
		if (ffpro == 3)
			pro_proc3();
		if (ffpro == 4)
			pro_proc4();
	}

	if ( fftoo)
	{
		if (fftoo == 1)	
			pro_tool1();
		if (fftoo == 2)
			pro_tool2();
		if (fftoo == 3)
			pro_tool3();
		if (fftoo == 4)
			pro_tool4();
		if (fftoo == 5)
			pro_tool5();
		if (fftoo == 6)
			pro_tool6();
		if (fftoo == 7)
			pro_tool7();
		if (fftoo == 8)
			pro_tool8();
		if (fftoo == 9)
			pro_tool9();
	}

	if ( ffuti)
	{
		if (ffuti == 1)	
			pro_util1();
		if (ffuti == 2)
			pro_util2();
		if (ffuti == 3)
			pro_util3();
		if (ffuti == 4)
			pro_util4();
		if (ffuti == 5)
			pro_util5();
		if (ffuti == 6)
			pro_util6();
		if (ffuti == 7)
			pro_util7();
		if (ffuti == 8)
			pro_util8();
		if (ffuti == 9)
			pro_util9();
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	proc_principal
 *
 * 	parser1
 * 	parser de linea de texto
 *	el viejo y querido parser de tesis version I !!
 *
 * -----------------------------------------------------------------------------------
 */

int	proc_principal()
{

	int	px,py;


	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	tk[MAXT][MAXB];

	char	z[MAXV];
	sprintf (z,"bienvenido al parser !!!");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp)
		gp_uso(101);

/* parser */
/* parser */
#if 1

	/* si encuentro chars no detect por el parser, avisar al final del proceso */
	flag_caracteres = 0;

	q_lin=0;
	while (fgets(b1, MAXB, hfinp) != NULL)
	{

	    /* procesar solo lineas no vacias */
#if 0
	    /* opcion - no proceso lineas vacias */
	    if ( !linea_vacia(b1))
#endif

	    /* opcion - proceso todas las lineas  */
	    if ( 1 )
	    {
		/* blancos al final */
		for (i=strlen(b1)-1, f4=1 ; i && f4 ; i-- )
			if (b1[i] == ' ' || b1[i] == '\n' )
				b1[i]=0;
			else
				b1[i+1]='\n', b1[i+2]=0, f4=0;

 
		f1=1;

		if (gp_fverbose("d3"))
		{
			printf ("Linea  : %d \n\n",q_lin);
			printf ("Buffer :|%s|\n\n",b1);
		}



		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			/* controlamos cantidad de tokens ... */
			if (q_tk > MAXT-10)
			{	error(501);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
					}
					p1++; 
					break; 

				/* letras */
				case TC_LET:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_LET || \
						(j == TC_NUM && !char_demed(b1[p1-1])  ) )
						tk[q_tk][p2++]=b1[p1++];
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* numeros tenemos que contemplar 3.3 o 3,3 !! */
				case TC_NUM:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_NUM || \
						(tipo_char(b1[p1]) == TC_PNT && tipo_char(b1[p1+1]) == TC_NUM ) || \
						( (b1[p1]) == ',' && tipo_char(b1[p1+1]) == TC_NUM ) )
					{	tk[q_tk][p2]=b1[p1];
						p1++;
						p2++;
					}
					tk[q_tk][p2]=0;
					q_tk++;
					break;


				/* blanco o tab */
				case TC_BLA:
#if 0
					/* opcion: no los guardo */
					while ( (j=tipo_char(b1[p1])) == TC_BLA)
					       p1++;	
#endif
					/* opcion: los guardo */
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;

					break;

				/* coma */
				case TC_CCE:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* punto */
				case TC_PNT:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis abre */
				case TC_PAA:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis cierra */
				case TC_PAC:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				case TC_EOL:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;
	
					/* caracteres varios */
					case TC_CVR:
						tk[q_tk][0]=b1[p1];
						tk[q_tk][1]=0;
						q_tk++;
						p1++;
						break;
	
					/* fin de linea */
					default:
						printf ("Default, algo salio mal  !!!\n\n");
						f1=0;
						break;
				}
	
			} /* while */
	
	
	
			if (gp_fverbose("d3"))
				printf ("termine de parsear file \n");
	
	
			/* verifico si hay que sacar output  en minusculas */
			if (gp_minusculas)
			{
				for (j=0; j< q_tk; j++)
					strcpy(tk[j],pasar_a_minusc(tk[j]));
			}
	
	#if 0
			/* si esta usando tabla de marcas ... verificar si hay que taggear */
			if (gp_tabmrk)
			{
				for (j=0; j< q_tk; j++)
					if (es_word(tk[j]) || es_puntuacion(tk[j]) )
						strcpy(tk[j],bm_tag(tk[j]));
			}
	#endif
	
	
	
			/* hay que forzar  string EOL al final de la linea */
			if (gp_eol)
				strcpy(tk[q_tk++],"EOL");
	
	
			/* salida en formato token columnar */
			if (gp_fsentencia == 0)
			{
	
	
				/* pidio nivel de descripcion en salida ... agrego la sentencia */
				if (gp_niveldes)
				{
					if (ffout)
						fprintf (hfout,"%s\n",b1);
				}	
	
				/* grabo los tokens encontrados */
				for (j=0; j< q_tk; j++)
				{
					switch (gp_niveldes)
					{
	
						case 0:
							if (ffout)
								fprintf (hfout,"%s\n",tk[j]);
							break;
	
						case 1:
							if (ffout)
								fprintf (hfout,"%3d,%s\n",j,tk[j]);
							break;
	
						default:
							if (ffout)
								fprintf (hfout,"%s\n",tk[j]);
							break;
					}
	
	
					if (gp_fverbose("d1"))
						printf ("%3d,%s\n",j,tk[j]);
	
				}
	
	
	
	#if 0
				/* agrego termino EOL para indentificar donde termino
				 * esto ya que tal vez haya un . en mitad de linea 
				 */
				if (gp_eol)
				{	if (ffout)
						fprintf (hfout,"%s\n","EOL");
				}
	#endif
			} /* if gp sentencia */
	
	
	
		/* salida en formato sentencia */
		if (gp_fsentencia == 1)
		{


			/* grabo los tokens encontrados */
#if 0
			if (ffout)
				fprintf (hfout,"%s\n",b1);
#endif

			for (j=0; j< q_tk; j++)
			{
#if 0
				if (ffout)
					fprintf (hfout,"%3d,%s\n",j,tk[j]);
#endif
				if (ffout)
					fprintf (hfout,"%s",tk[j]);

				if (gp_fverbose("d1"))
					printf ("%3d,%s\n",j,tk[j]);

			}

#if 0
			/* agrego termino EOL para indentificar donde termino
			 * esto ya que tal vez haya un . en mitad de linea 
			 */
			if (gp_eol)
			{	if (ffout)
					fprintf (hfout,"%s\n","EOL");
			}
			else
			{	if (ffout)
					fprintf (hfout,"\n");
			}
#endif


			/* se termino la linea */
			if (ffout)
				fprintf (hfout,"\n");

		}


		if (gp_niveldes)
			fprintf (hfout,"\n\n");


		if (gp_fverbose("d1"))
		{
			printf ("\n");
		}




		/* 
		 * Termine todo lo que tenia que hacer con esta linea,
		 * sumo lineas 
		 *
		 */

		q_lin++;


	    } /* if ... no esta vacia la linea */

	}  /* while fgets ... */


	
	if (gp_fverbose("d1"))
	{
		printf ("Cant de lineas procesadas %d\n",q_lin);
		printf ("\n\n\n");
	}


	/* parser1 */




#endif
/* parser */
/* parser */


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}
	
	

/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util 1
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util
 *
 *	La idea es generar un numero adimensional, que de pauta de la complejidad
 *	de los archivos en el codebase.
 *	De esta manera, los podemos ordenar por complejidad,
 *	de manera de ir armando una gramatica, comenzando siempre por los 
 *	mas sencillos.
 *
 */


int	pro_util1()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	flag;
	int	ql,qlf,q_ptr;
	int	x1;
	
	char	b0[MAXB];
	char	b1[MAXB];
	char	d1[MAXB];
	char	d2[MAXB];

	FILE	*hwi,*hwo;

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util1");
	w = g_dbg(z);


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffaux )
		gp_uso(105);
	if (!ffsta)
		agregar_ffsta("tool.sta");


	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}

		printf ("\n");
	}
#endif
	printf ("\n");


	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	if (gp_suboption == 0 )
	{
		/* hacer algo con todas las lineas */
		eu1_p1();
	}


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(607);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	mostrar_reportes("util1");


if (gp_debug && z)
{
	for (i=0; i<qf_ff; i++)
	{
		mprintf (z,"|%-40.40s|\n",(*tb[i]).n);

		fprintf (hfout ,"%4d %-40.40s %3d\n",
			i,(*tb[i]).n,i);
	}
}
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}


	
/*
 * -----------------------------------------------------------------------------------
 *
 *	eu1_p1 
 *
 * -----------------------------------------------------------------------------------
 */

int	eu1_p1()
{


}



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util2
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util2
 */

int	pro_util2()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util2");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util3
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util3
 */

int	pro_util3()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util3");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util4
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util4
 */

int	pro_util4()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util4");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util5
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util5
 */

int	pro_util5()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util5");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util6
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util6
 */

int	pro_util6()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util6");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}


	
/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util7
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util7
 */

int	pro_util7()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util7");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util8
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util8
 */

int	pro_util8()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util8");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_util9
 *
 * -----------------------------------------------------------------------------------
 */

/* 
 *	pro_util9
 */

int	pro_util9()
{
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	
	char	b0[MAXB];
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_util9");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso %s \n\n",gp_tm(),z);
	}
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale  proceso %s \n\n",gp_tm(),z);
	}
}





	
/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 1
 *
 *	exec aparte ...
 *
 * -----------------------------------------------------------------------------------
 */


/* 
 *	pro_exec1
 *
 *	cuenta caracteres de un archivo
 *
 */


int	pro_exec1()
{
	int	px,py;
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;
	
	int	tabla1[256];
	int	tabla2[256];

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	tk[MAXT][MAXB];

	char	mostrar[16];

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso exec 1 \n\n",gp_tm());
	}
		

	if (!ffinp || !ffout) 
		gp_uso(102);


	/* es uno o dos archivos */
	f4 = 0;
	if (ffin2)
		f4 = 1;

	/* init de valores */
	for (i=0; i<256; i++)
	{	tabla1[i]=0;
		tabla2[i]=0;
	}
	

	/* si encuentro caracteres no considerados para el parser, avisar al final de todo el proceso */
	flag_caracteres = 0;


	q_lin=0;
	while (fgets(b1, MAXB, hfinp) != NULL)
	{

	    /* opcion - proceso lineas vacias */
	    if ( 1 )
	    {
 
		if (gp_fverbose("d3"))
		{
			printf ("Linea  : %d \n\n",q_lin);
			printf ("Buffer :|%s|\n\n",b1);
		}

		f1=1;


		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			j=tipo_char(b1[p1]);
			tabla1[b1[p1]]++;
			
			if (b1[p1] == 0)
				f1=0;
			else
				p1++;

		} /* while */


		if (gp_fverbose("d3"))
			printf ("termine de parsear fila \n");
	   }

	   q_lin++;
	}
	
	
	
	/* cuento chars de segundo archivo */
	if (f4)
	{
		q_lin=0;
		while (fgets(b1, MAXB, hfin2) != NULL)
		{
	
		    /* opcion - proceso lineas vacias */
		    if ( 1 )
		    {
	 
			if (gp_fverbose("d3"))
			{
				printf ("Linea  : %d \n\n",q_lin);
				printf ("Buffer :|%s|\n\n",b1);
			}
	
			f1=1;
	
	
			/* comienzo parser de tokens */
			p1=0;
			q_tk=0;
	
			while ( f1 )
			{
				j=tipo_char(b1[p1]);
				tabla2[b1[p1]]++;
				
				if (b1[p1] == 0)
					f1=0;
				else
					p1++;
	
			} /* while */
	
	
			if (gp_fverbose("d3"))
				printf ("termine de parsear fila \n");
		   }
	
		   q_lin++;
		}
	}


	/* grabo resultados */

	for (j=0; j< 256; j++)
	{
		memset (mostrar,0,16);
		sprintf (mostrar,"%c",j);
		if (j == 8)
			sprintf (mostrar,"%s","Backspace");
		if (j == 9)
			sprintf (mostrar,"%s","TAB");
		if (j == 10)
			sprintf (mostrar,"%s","Line Feed");
		if (j == 11)
			sprintf (mostrar,"%s","Vertical Tab");
		if (j == 12)
			sprintf (mostrar,"%s","Form Feed");
		if (j == 13)
			sprintf (mostrar,"%s","Carriage Return");
		if (j == 27)
			sprintf (mostrar,"%s","Escape");
		if (j ==127)
			sprintf (mostrar,"%s","Delete");


		if (!f4)
			sprintf (b1,"Char %3d %7d   |%s|\n",j,tabla1[j],mostrar);
		else
			sprintf (b1,"%sChar %3d %7d %7d     |%s| \n", (abs(tabla1[j]-tabla2[j]) ? "X " : "  "),j,tabla1[j],tabla2[j] ,mostrar);


		fprintf (hfout,"%s",b1);

		if (gp_fverbose("d1"))
			printf ("%s",b1);
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale proceso exec 1 \n\n",gp_tm());
	}


}




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 2
 *
 *	
 *
 * -----------------------------------------------------------------------------------
 */



#if 0

int	pro_exec2()
{

	/* exec */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a proceso exec 2 \n\n",gp_tm());
	}
		

	/* exec */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de proceso exec 2 \n\n",gp_tm());
	}

}

#endif








/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 2
 *
 *	proceso de series de archivos indicados por lista 
 *
 * -----------------------------------------------------------------------------------
 */

#if 1

/*
 *
 *	viene de prue2
 *
 *	abre archivo con lista de archivos a procesar (fuentes fortran)
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 */


#if 0

#define	MAX_QSRC		500		* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec2()
{

	int	i,j,k;
	char	d1[MAXB];
	char	d2[MAXB];
	int	ql,qlf;
	int	flag;
	int	q_ptr;

	FILE	*hwi,*hwo;


	char	z[MAXV];
	sprintf (z,"exec2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffdat )
		gp_uso(103);



	/* cantidad de lineas en el archivo  */
	qf_ff = 0;
	q_ptr = 0;
	ql=0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);

			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(903);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;
			(*tb[qf_ff]).ql = qlf;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
			ql++;
		}
	}


	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de lineas cargadas:  %5d \n",ql);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/* proceso todos los files */
#if 1
	ps_src1();
#endif


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(604);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}

	fclose(hwo);

		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



#endif



/*
 * -----------------------------------------------------------------------------------
 *
 *	ps_src1  
 *
 *	proceso un file
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	procesar todos los fuentes
 *
 *	OJO: esto quedo trunco ...
 *	proc4 ... carga todos los files como cadenas a estructuras ...
 *	y despues, vimos que procesar archivos con esa estructura ...
 *	se torna muy complicado...
 *	no es facil acceder a lin[var] ... lin[var+1] etc 
 */


int	ps_src1()
{

	int	ql_ini,ql_fin;
	int	i,j,k,l;
	int	f1,f2,f3,f4;
	int	pf,uf,ln;

	char	b1[MAXB];
	char	b2[MAXB];

	char	s1[MAXB];
	char	s2[MAXB];

	/* chequeo que esta todo bien */
	if (gp_fverbose("d3"))
	{
		printf ("\n\n\n");
	
		for (i=0; i< qf_ff; i++)
		{
			printf ("FF: %3d  %5d  |%s|\n", i, (*tb[i]).ql, (*tb[i]).n );
		}

		printf ("\n\n\n");
	}




	for (i=0; i< qf_ff; i++)
	{
		strcpy(b1, (*tb[i]).n);
		pf = (*tb[i]).pf;
		uf = (*tb[i]).uf;

		for (ln=pf; ln<=uf; ln++)
		{

			l_pars(ln,&q_tk );
			strcpy (b2, (*fnp[ln]).l );

			if (tiene_include_valido(b2,s1,s2))
			{
#if 0
				fprintf (hfout,"%-30.30s %-30.30s \n",b1,s1  );
#endif
				fprintf (hfout,"%-30.30s %-30.30s \n",s1,b1  );

			}

		}

	}

}


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_include_valido  
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	tiene_include_valido(s,l1,l2)
char	*s;
char	*l1;
char	*l2;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	n_inc;
	int	n_ap1,n_ap2;
	char	b1[MAXB];


	/* mientras true, hay esperanza */
	f1 = 1;

	/* si es comentario, no va */
	if (f1)
	{
		if (es_linea_comentario(s))
			f1 = 0;
	}

	/* si no figura la palabra include, no va */
	if (f1)
	{
		for (i=0, f2=0; !f2 && i<q_tk; i++)
		{
			if (!strcmp("include",pasar_a_minusc(tk[i])))
				f2 = 1, n_inc=i;
		}

		if (!f2)
			f1 = 0;	
	}

	/* si antes del include hay algo distinto a blancos, no va */
	if (f1)
	{
		for (i=0; f1 && i< n_inc && i< q_tk; i++)
			if (strcmp(" ",tk[i]))
				f1 = 0;

	}


	/* si despues del include, no hay blancos y un "'",  no va */
	if (f1)
	{	
		i = n_inc+1;
		while (tk[i][0] == ' ' && i< q_tk )
			i++;

		if (tk[i][0] == '\'')
			n_ap1 = i;
		else	
			f1 = 0;

	}


	/* si encontro un "'" ... veamos */
	if (f1)
	{
		if (tk[n_ap1+2][0] == '.')
			sprintf (b1,"%s.%s",tk[n_ap1+1],tk[n_ap1+3]);
		else
			f1 = 0;
	}


	if (f1)
	{
		if (es_nombre_de_include(b1) )
			strcpy(l1,b1);
		else
			f1 = 0;
	}


	return (f1);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	es_nombre_de_include  
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	es_nombre_de_include(s)
char	*s;
{

	int	i,j,k;
	int	f1,f2;
	char	c;


	f1 = 1;

	for (i=0; f1 && i<strlen(s); i++)
	{
		c = s[i];

		if (! ( c == '.' || c == '_' || ( c >= 'A' && c <= 'Z' ) || ( c >= 'a' && c <= 'z' ) || ( c >= '0' && c <= '9') ))
			f1 = 0;
	}

	return (f1);
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 3
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec3()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffdat )
		gp_uso(104);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	ex3_p2();
	ex3_p3();
	ex3_p4();


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(602);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 4
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec4()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffaux )
		gp_uso(105);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	ex4_p1();


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(606);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfaux    source - include (resumen completo)
 */

int	ex3_p2()
{
	int 	i,j,k;
	int	f1,f2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf,ql;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	info[30][MAXB];
	int	s_info[30];
	char	nro[16];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;
		ql = uf - pf + 1;

		if (es_mon(b3))
			f_mon = 1;

		n_info = 0;
		sprintf (info[n_info],"%-34.34s",b3);

		if (f_mon)
		{	complejidad_mon(pf,uf,&m_nl,&m_nc);
			sprintf (nro2,"%3d %2d",m_nl,m_nc);
			sprintf (info[n_info],"%-34.34s                             %3d %2d",b3,m_nl,m_nc);
			info[n_info][60] = 'M';
		}

			

		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
			{
#if 0
				fprintf (hflog,"%-34.34s  %s\n", b3, b2);
				fprintf (hfout,"%-15.15s  %s\n", b2, b3);
#endif

				n_info++;
				sprintf (info[n_info],"%-34.34s %s",b3,b2);
			}



		}

		fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");

		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp1 );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}

	}

	fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfout ... include - file
 *
 */

int	ex3_p3()
{
	int 	i,j,k;
	int	f1,f2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	info[700][MAXB];
	int	s_info[30];
	char	nro[16];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	n_info = 0;

	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

			
		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
			{

				sprintf (info[n_info],"%-16.16s %s",b2,b3);
				n_info++;
			}



		}

#if 0

		fprintf (hfout,"%s\n","-------------------------------------------------------------------------");

		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}

#endif

	}

	qsort ( &info[0],n_info,MAXB,info_cmp2);

	for (k=0; k < n_info; k++)
	{

		fprintf (hfout,"%s\n",info[k]);
	}

}







/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfout ... file - include
 *
 */

int	ex3_p4()
{
	int 	i,j,k;
	int	f1,f2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	info[700][MAXB];
	int	s_info[30];
	char	nro[16];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	n_info = 0;

	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

#if 0
		if (es_mon(b3))
			f_mon = 1;

		n_info = 0;
		sprintf (info[n_info],"%-34.34s",b3);

		if (f_mon)
		{	complejidad_mon(pf,uf,&m_nl,&m_nc);
			sprintf (nro2,"%3d %2d",m_nl,m_nc);
			sprintf (info[n_info],"%-34.34s                             %3d %2d",b3,m_nl,m_nc);
			info[n_info][60] = 'M';
		}
#endif

			
		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
			{
#if 0
				fprintf (hflog,"%-34.34s  %s\n", b3, b2);
				fprintf (hfout,"%-15.15s  %s\n", b2, b3);
#endif

				sprintf (info[n_info],"%-36.36s %s",b3,b2);
				n_info++;
			}



		}

#if 0



		fprintf (hfout,"%s\n","-------------------------------------------------------------------------");

		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}
#endif 

	}

	qsort ( &info[0],n_info,MAXB,info_cmp2);

	for (k=0; k < n_info; k++)
	{

		fprintf (hflog,"%s\n",info[k]);
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex4_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec4
 * hace algo con todas las lineas cargadas en memoria
 *
 * hfaux    source - entrys ....
 *
 */

int	ex4_p1()
{
	int 	i,j,k,l;
	int	f1,f2;
	int	p1,p2;
	int	f_mon,m_nl,m_nc;
	int	n_f;
	int	pf,uf,uuf;
	int	n_info;
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	info[300][MAXB];
	int	s_info[300];
	char	nro[128];
	char	nro2[16];

	for (i=0; i<30; i++)
		s_info[i]=0;


	uuf = (*tb[qf_ff-1]).uf;

	/* para todos los archivos  */
	for (j=0; j < qf_ff; j++)
	{
		f_mon = 0;

		strcpy (b3, (*tb[j]).n );
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

		if (es_mon(b3))
			f_mon = 1;

printf ("ex4_p1: proceso file %2d |%s| \n",strlen(b3),b3);

		n_info = 0;
		sprintf (info[n_info],"%-34.34s",b3);

			

		for (i = pf; i <= uf; i++)
		{
			strcpy(b1,(*fnp[i]).l );

			if (!es_linea_comentario(b1) && tiene_entry_v1(b1,b2) && !primer_caracter_es_mas(b1) )
			{
				n_info++;
				strcpy(info[n_info],b2);

#if 0
				sprintf (info[n_info],"%-20.20s %3d %s"," ",n_info,b2);
#endif
printf ("file: %-30.30s entry: %s \n",b3,b2);
			}



		}

		fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");


		strcpy(nro," ");
		if (n_info)
			sprintf (nro,"%-20.20s (%3d)"," ",n_info);
		strcat(info[0],nro);

		fprintf (hfaux,"%3d %s\n",lne(j),info[0]);
		if (n_info)
		{
			for (k=1; k <= n_info; k++)
			{
#if 0
				fprintf (hfaux,"%s\n",info[k]);
#endif
				fprintf (hfaux,"%-3.3s %3d %s\n"," ",k,info[k]);

#if 1
				/* loop machazo */
printf ("busco call |%s| \n",info[k]);

				for (l=0; l< uuf; l++)
				{
					strcpy(b4,(*fnp[l]).l );
					if (!es_linea_comentario(b4) && tiene_call_a_entry(b4,info[k]) )
					{
						primer_char(b4,&p1);
						strcpy(b5,f_name(l));
						fprintf (hfaux,"%-8.8s%-20.20s |%s\n"," ",b5,b4+p1);
					}

					strcpy(b4,(*fnp[l]).l );
					if (!es_linea_comentario(b4) && tiene_funcion_a_entry(b4,info[k]) )
					{
						strcpy(b6,minusculas(b4));

						if (!tiene_entry(b6) )
						{
						primer_char(b4,&p1);
						strcpy(b5,f_name(l));
						fprintf (hfaux,"%-8.8s%-20.20s |%s\n"," ",b5,b4+p1);
						}
					}





				}
#endif

			}
		}


#if 0
		if (n_info == 0)
			fprintf (hfaux,"%3d %s\n",lne(j),info[n_info]);
		else
		{
			s_info[1]=1;
			if (n_info > 1)
			{
				qsort ( &info[1],n_info,MAXB,info_cmp1 );

				for (k=1; k <= n_info; k++)
				{

					if ( k == 1 )
					{
						s_info[k] = 1;
					}

					if ( k != 1 )
					{
						if ( !strcmp(info[k],info[k-1]) )
						{
							s_info[k] = s_info[k-1] + 1;
							s_info[k-1] = 0;
						}
						else
							s_info[k] = 1;
					}
				}

					
			}

			for (k=1, f2=1; k <= n_info; k++)
			{
				if (s_info[k])
				{	
					strcpy(nro,"   ");
					if (f2)
					{	sprintf (nro,"%3d",lne(j) );
						f2 =0;
					}

					if (f_mon)
						info[k][60]='M';

					fprintf (hfaux,"%s %-50.50s (%2d)\n",nro,info[k],s_info[k]);
				}
			}
		}
#endif

	}

	fprintf (hfaux,"%s\n","-------------------------------------------------------------------------");
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 5
 *
 *	lista variables de fortran que tienen blancos intermedios 
 *
 * -----------------------------------------------------------------------------------
 */



/* 
 *	pro_exec5
 *	analiza include files como mthnmcom y namescom
 *	lista variables de fortran que tienen blancos intermedios 
 *
 */


int	pro_exec5()
{
	int	px,py;
	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin,q_var;
	int	q_ptr;
	int	q_tk;
	int	p1,p2,p3,p4;
	int	pf,uf;
	int	sw1 = 0;		/* 0 grabo archivo normal, 1 grabo con marcas */
	int	es_int;
	int	es_chr;
	int	qv1,qv2;
	int	q1,q2;
	int	f_sigo;
	
	int	tabla1[256];
	int	tabla2[256];

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	tk[MAXT][MAXB];

	char	mostrar[16];

	char	z[MAXV];
	sprintf (z,"exec5");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		

	/* que la vida sea mas leve */
	if (!ffaux)
		agregar_ffaux("parser.err");

	if (!ffinp || !ffout || !fflog || !ffou3 || !ffou4) 
		gp_uso(102);



	/* cantidad de lineas  cargadas  */
	q_ptr = 0;

	fnq1 = &fnp[q_ptr];
	qfv_load(hfinp,fnq1,&q_lin);

	printf ("Cantidad de lineas cargadas   :  %5d \n",q_lin);
	printf ("\n");


#if 1


	q_var = 0;
	pf = 0;
	uf = q_lin -1;
	sprintf (b3,"      integer(kind=2),parameter:: ");
	sprintf (b5,"      character(len=1),parameter:: ");
	sprintf (b6,"      integer(kind=4),parameter:: ");


	qv1 = 0;
	qv2 = 0;

	/* reviso cada linea marco sobre las que hay que trabjar */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b1, (*fnp[ j ]).l );
		strcpy (b2, (*fnp[ j ]).l );
		(*fnp[ j ]).f1 = 0;

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		/*
		 * detectar si la linea es comentarios ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		f4 = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f4 = 0;

		/* solo proceso lineas que no son comentario */
		if (f4 && ( tiene_integer(b1) || tiene_character(b1) ) )
		{	
			es_int = 0;
			es_chr = 0;

			/* atencion ! big tema aca !!!
			 * usamos el mismo proceso para dos files con diferentes variables !!!
			 * un file tiene integer (kind=4) ... no se detecto !! 
			 */

			if (tiene_integer(b1))
				es_int = 1;
			if (tiene_character(b1))
				es_chr = 1;

			/* ojo
			 * asi como esta, funciona solo para len=1 y kind=2 
			 *
			 * por el moemento, si no es asi, dejamos error ...
			 */


#if 0
			if (tiene_integer(b1) && !es_integer2(b1))
				error(6101);
			if (tiene_character(b1) && !es_character1(b1))
				error(6102);
#endif

			(*fnp[ j ]).f1 = 1;
			(*fnp[ j ]).f2 = 0;

			for (i=0, f2 = 1, f3 = 0, p1 = 0; f2 && i < strlen(b1); i++)
			{
				if (!strncmp(b1+i,"::",2))
				{	strcpy(b4,b1+i+3);
					f2 = 0;
					f3 = 1;
				}
			}

			if (f3 == 0)
				error(5001);


			if (es_int)
				strcpy(b1,b3);
			if (es_chr)
				strcpy(b1,b5);

			strcat(b1,b4);

			/* tiene blancos intermedios ... */
			for (i=0, f2 = 1, f3 = 0; f2 && i< strlen(b4); i++)
			{
				if (b4[i] == '=')
				{
					f2 = 0;
					f3 = 1;

					b4[i] = 0;
					if (b4[i-1] == ' ')
						b4[i-1] = 0;

			printf ("XXX b4: |%s| \n",b4);


				}
			}

			if (f3 == 0)
				error(5002);

			for (i=0, f2=0, f3=0; i<strlen(b4); i++)
				if ( b4[i] == ' ')
				{
					f3 = 1;
					f2++;
					(*fnp[ j ]).f2 = 1;

				}

			/* grabo la variable pura */
			if (f3 && ffou2)
			{
				fprintf (hfou2,"|%s|\n",b4);

				sprintf (vcb[qv1],"|%s|",b4);
				qv1++;
			}

			/* grabo la lista de variables con _ */
			if (!f3 && fflog)
			{
				fprintf (hflog,"|%s|\n",b4);
			}
			
			q_var++;
		}

		strcpy  ( (*fnp[ j ]).l, b1 );
	}


	printf ("Cantidad de lineas no comentarios  :  %5d \n",q_var);
	printf ("\n");



#endif



#if 1
	/* grabo resultados */
	sw1 = 0;

	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		strcpy (b1, (*fnp[ j ]).l );

		if ( (*fnp[ j ]).f1 == 0)
		{
			fprintf (hfout,"%s\n",b1);
		}

		if ( (*fnp[ j ]).f1 == 1 && (*fnp[ j ]).f2 == 0)
		{
			if (sw1)
			{
				b1[0] = 'x';
			}
			fprintf (hfout,"%s\n",b1);
		}
		 
		if ( (*fnp[ j ]).f1 == 1 && (*fnp[ j ]).f2 == 1)
		{
			if (sw1)
			{
				b1[0] = 'x';
				b1[1] = '2';
			}
			fprintf (hfout,"%s\n",b1);
		}
		 

		if (gp_fverbose("d1"))
			printf ("%s\n",b1);
	}
#endif

	/* ordeno las variables vcb segun la cantidad de blancos que tengan
	 * y grabo
	 */

	if (ffou3)
	{
		qv2 = 0;

		for (i=6; i; i--)
		{
			for (j=0; j<qv1; j++)
			{
				for (k=0, q1=0; k<strlen(vcb[j]) ; k++)
					if (vcb[j][k] == ' ')
						q1++;
				
				if (i == q1)
				{
					strcpy(vcb_o[qv2],vcb[j]);
					qv2++;
				}	
			}
		}


		for (j=0; j<qv1; j++)
		{
			fprintf (hfou3,"%s\n",vcb_o[j]);
		}
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}


/*
 * -----------------------------------------------------------------------------------
 *
 *	es_integer2  
 *
 *
 * -----------------------------------------------------------------------------------
 */





int	es_integer2(s)
char	*s;
{
	int	i;
	int	f_resultado, f_sigo;
	char	target[MAXB];

	strcpy(target,"kind=2");

	f_resultado = 0;
	for (i=0, f_sigo=1; f_sigo && i<strlen(s); i++)
	{
		if (!strncmp(s+i,target,strlen(target)))
			f_sigo = 0, f_resultado = 1;
	}

	return (f_resultado);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	es_character1  
 *
 *
 * -----------------------------------------------------------------------------------
 */

int	es_character1(s)
char	*s;
{
	int	i;
	int	f_resultado, f_sigo;
	char	target[MAXB];

	strcpy(target,"len=1");

	f_resultado = 0;
	for (i=0, f_sigo=1; f_sigo && i<strlen(s); i++)
	{
		if (!strncmp(s+i,target,strlen(target)))
			f_sigo = 0, f_resultado = 1;
	}

	return (f_resultado);
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_integer  
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	tiene_integer(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	f_tiene;
	char	b1[MAXB];

	strcpy(b1,pasar_a_minusc(s));

	for (i=0, f1=1, f_tiene=0; f1 && i < strlen(b1); i++)
	{
		if (!strncmp(b1+i,"integer",7))
		{
			f1 = 0;
			f_tiene = 1;
		}
	}

	return ( f_tiene );
}

	
/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_character  
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	tiene_character(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	f_tiene;
	char	b1[MAXB];

	strcpy(b1,pasar_a_minusc(s));

	for (i=0, f1=1, f_tiene=0; f1 && i < strlen(b1); i++)
	{
		if (!strncmp(b1+i,"character",9))
		{
			f1 = 0;
			f_tiene = 1;
		}
	}

	return ( f_tiene );
}

	




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 6
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 6
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 *
 *	proceso para determinar variables con blancos intermedios
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec6()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec6");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffin2 || !ffin3 || !ffaux || !ffout || !ffou4 || !ffou5 || !ffou6  )
		gp_uso(105);



	/* cargo las variables vcb */	
	q_vcb = 0;

	while (fgets(b1,MAXV,hfin2) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}

			/* saco el ultimo | */
			if (b1[strlen(b1)-1] == '|')
				b1[strlen(b1)-1] = 0;
			else
				error(5003);

			/* copio desde el primer caracter despues del primer | */
			strcpy(vcb[q_vcb],b1+1);

printf ("XXX cargue vcb |%s| \n",vcb[q_vcb]);

			q_vcb++;
		}
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de vcb cargadas : %6d\n",q_vcb);
	}



	/* cargo las variables vsb */	
	q_vsb = 0;

	while (fgets(b1,MAXV,hfin3) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}

			/* saco el ultimo | */
			if (b1[strlen(b1)-1] == '|')
				b1[strlen(b1)-1] = 0;
			else
				error(5004);

			/* copio desde el primer caracter despues del primer | */
			strcpy(vsb[q_vsb],b1+1);

printf ("XXX cargue vsb |%s| \n",vsb[q_vsb]);

			q_vsb++;
		}
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de vsb cargadas : %6d\n",q_vsb);
	}





	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	if (gp_suboption == 1 )
	{
		/* hace el cambio de variables con blancos x misma version con _ */
		ex6_p1();
	}


	if (gp_suboption == 2 )
	{
		/* chequeo ... estas variables con blancos ... estan en otros fuentes que no t el include ? */
		ex6_p2();
	}

	if (gp_suboption == 3 )
	{
		/* chequeo ... estas variables con blancos reemp x _, ... estan en los fuentes que  t el include ? */
		ex6_p3();
	}

	if (gp_suboption == 4 )
	{
		/* chequeo ... estas variables con blancos reemp x _, ... estan en los fuentes que  t el include ? */
		ex6_p4();
	}

	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(603);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v1
 * toma las variables vcb (var con blanks )
 * y reemplaza por la misma con '_'
 * trabaja sobre todos los fuentes cargados en vector 
 * genera version modificada en repo indicado
 *
 */

int	ex6_p1()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5,f_var_ok;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;

	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		strcpy (b2, pasar_a_minusc(b1));
		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);

		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							f_var_ok = 1;

							printf ("VW1 lin : |%s|\n",b2);
							printf ("VW2 var : |%s|\n",b3);
							printf ("VW3 cn1 : (%6d)\n",k1-1);
							printf ("VW4 cn2 : (%6d)\n",k1+strlen(b3));
							printf ("VW5 ca1 : (%c) \n",b2[k1-1]);
							printf ("VW6 ca2 : (%c) \n",b2[k1+strlen(b3)]);
							printf ("VW7 . . .  \n");


							/* la variable esta, pero dentro de un string ! 
							 * esto detecta lo simple :
							 * 'variable' ...
							 * pero faltaria que detecte 
							 * 'la variable esta dentro de mas caracteres '
							 */


							/* 'variable' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == '\'')
							{
								f_var_ok = 0;
							}

							/* "variable" */
							if (b2[k1-1] == '\"' && b2[k1+strlen(b3)] == '\"')
							{
								f_var_ok = 0;
							}

							/* 'variable,' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == ',' && b2[k1+strlen(b3)+1] == '\'')
							{
								f_var_ok = 0;
							}

							/* ' variable' */
							if (b2[k1-1] == ' ' && b2[k1+strlen(b3)] == '\'')
							{
								f_var_ok = 0;
							}

							/* 'variable ' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == ' ')
							{
								f_var_ok = 0;
							}

							/* 'variable-cr' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == '-' 
									     && b2[k1+strlen(b3)+1] == 'c' && b2[k1+strlen(b3)+2] == 'r')
							{
								f_var_ok = 0;
							}


							/* 'variable-dr' */
							if (b2[k1-1] == '\'' && b2[k1+strlen(b3)] == '-' 
									     && b2[k1+strlen(b3)+1] == 'd' && b2[k1+strlen(b3)+2] == 'r')
							{
								f_var_ok = 0;
							}




							if (f_var_ok)
							{

							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 - - - - - - \n");

							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;


							}



							}
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



	for (i=0; i<qm1; i++)
		printf ("%4d |%s|\n",i,vcb_p[i]);



	qsort(&vcb_o[0],qv1, MAXV, compare_vcb);

#if 1
    // Contador para elementos únicos
    int unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qv1; ++i) 
    {
	// Compara el elemento actual con el anterior
	if (strcmp(vcb_o[i], vcb_o[i - 1]) != 0) 
	{
	    // Si son diferentes, mueve el elemento único al índice de `unique_count`
	    strcpy(vcb_o[unique_count], vcb_o[i]);
	    ++unique_count;
	}
    }

    // Actualiza la cantidad de elementos únicos
    qv2 = unique_count;
#endif


	if (ffout)
	{
		for (i=0; i<qv2; i++)
			fprintf (hfout,"|%s|\n",vcb_o[i]);
	}



	qsort(&vcb_p[0],qm1, MAXB, compare_vcb);

#if 1
    // Contador para elementos únicos
    unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qm1; ++i) 
    {
	// Compara el elemento actual con el anterior
	if (strcmp(vcb_p[i], vcb_p[i - 1]) != 0) 
	{
	    // Si son diferentes, mueve el elemento único al índice de `unique_count`
	    strcpy(vcb_p[unique_count], vcb_p[i]);
	    ++unique_count;
	}
    }

    // Actualiza la cantidad de elementos únicos
    qm2 = unique_count;
#endif




	if (ffou4)
	{
		for (i=0; i<qm2; i++)
			fprintf (hfou4,"%s\n",vcb_p[i]);
	}



	qsort(&vcb_c[0],qm1, MAXB, compare_vcb);

#if 1
    // Contador para elementos únicos
    unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qm1; ++i) 
    {
	// Compara el elemento actual con el anterior
	if (strcmp(vcb_c[i], vcb_c[i - 1]) != 0) 
	{
	    // Si son diferentes, mueve el elemento único al índice de `unique_count`
	    strcpy(vcb_c[unique_count], vcb_c[i]);
	    ++unique_count;
	}
    }

    // Actualiza la cantidad de elementos únicos
    qm2 = unique_count;
#endif

	qsort(&vcb_d[0],qm1, MAXB, compare_vcb);

#if 1
    // Contador para elementos únicos
    unique_count = 1;

    // Itera sobre el vector eliminando duplicados
    for (int i = 1; i < qm1; ++i) 
    {
	// Compara el elemento actual con el anterior
	if (strcmp(vcb_d[i], vcb_d[i - 1]) != 0) 
	{
	    // Si son diferentes, mueve el elemento único al índice de `unique_count`
	    strcpy(vcb_d[unique_count], vcb_d[i]);
	    ++unique_count;
	}
    }

    // Actualiza la cantidad de elementos únicos
    qm3 = unique_count;
#endif


	if (qm2 != qm3 )
		error(5555);




	if (ffou5)
	{
		for (i=0; i<qm2; i++)
			fprintf (hfou5,"%s\n",vcb_c[i]);

	}

	if (ffou6)
	{
		for (i=0; i<qm3; i++)
			fprintf (hfou6,"%s\n",vcb_d[i]);
	}





}

/*
 * -----------------------------------------------------------------------------------
 *
 *	compare_vcb   
 *
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	ejercicio de prueba
 */


#if 0
int compare_vcb(char *a, char *b) 
{
    return strcmp(a,b);
}
#endif


#if 0
int compare_vcb(a,b) 
const void *a;
const void *b;
{
    // Hacemos un cast a char * porque sabemos que estamos ordenando strings
    const char *ia = *(const char **)a;
    const char *ib = *(const char **)b;

    return strcmp(ia, ib);
}
#endif


#if 1
int	compare_vcb(p1,p2)
const	void	*p1;
const	void	*p2;
{
	int k;

	k = strcmp((const char *)p1,(const char *)p2);
	return k;
}

#endif



/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v2
 * toma las variables vcb (var con blanks )
 * y busca en todos los programas cargados
 * trabaja sobre todos los fuentes cargados en vector 
 * (la idea es buscar estas variables en programas que no tienen 
 * el include mthnmcom.mon )
 *
 */

int	ex6_p2()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;


	strcpy(base_name,"empty");

	qv1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );
		strcpy (b2, pasar_a_minusc(b1));
		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

							if (fflog)
							{
								fprintf (hfaux,"%-40.40s %05d |%s|\n",prog_name,lne(nf),b0);
								fprintf (hfaux,"%s","\n");
							}

						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v3
 * toma las variables vcb (var con blanks ) 
 * reemp blanks con _
 * y busca en todos los programas cargados
 * trabaja sobre todos los fuentes cargados en vector 
 * (la idea es buscar estas variables en programas que tienen 
 * el include mthnmcom.mon )
 *
 */

int	ex6_p3()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;


	memset(b4,'X',MAXB);
	strcpy(base_name,"empty");

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{

		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;

			printf ("Trabajo con ... |%s|\n",prog_name);
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );
		strcpy(b2,(*fnp[i]).l );
#if 0
		strcpy (b2, pasar_a_minusc(b1));
#endif
		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					/* copio la variable vcb a buscar */
#if 0
					strcpy(b3,pasar_a_minusc(vcb[k]));
#endif
					strcpy(b3,vcb[k]);
					strcpy(b5,vcb[k]);

					/* cambio blanks x underscore */
					for (k2 = 0; k2 < strlen(b3); k2++)
						if (b3[k2] == ' ')
							b3[k2] = '_';
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strncpy(b2+k1,b4,strlen(b3));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV6 \n");

							if (fflog)
							{
								fprintf (hfaux,"%-40.40s  var: |%s| \n",prog_name,b5);
								fprintf (hfaux,"(%5d) |%s|\n",lne(nf),b0);
								fprintf (hfaux,"%s","\n");
							}
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex6_p4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec6
 * hace algo con todas las lineas cargadas en memoria
 *
 * v4
 * toma las variables vcb (var con blanks ) 
 * reemp blanks con _
 * y busca en todos los programas cargados
 * trabaja sobre todos los fuentes cargados en vector 
 * (la idea es buscar estas variables en programas que tienen 
 * el include mthnmcom.mon )
 *
 */

int	ex6_p4()
{
	
}


#if 0

int	ex6_p4()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;


	memset(b4,'X',MAXB);
	strcpy(base_name,"empty");

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{

		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;

			printf ("Trabajo con ... |%s|\n",prog_name);
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );
		strcpy(b2,(*fnp[i]).l );
#if 0
		strcpy (b2, pasar_a_minusc(b1));
#endif
		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					/* copio la variable vcb a buscar */
#if 0
					strcpy(b3,pasar_a_minusc(vcb[k]));
#endif
					strcpy(b3,vcb[k]);
					strcpy(b5,vcb[k]);

					/* cambio blanks x underscore */
					for (k2 = 0; k2 < strlen(b3); k2++)
						if (b3[k2] == ' ')
							b3[k2] = '_';
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strncpy(b2+k1,b4,strlen(b3));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV6 \n");

							if (fflog)
							{
								fprintf (hfaux,"%-40.40s  var: |%s| \n",prog_name,b5);
								fprintf (hfaux,"(%5d) |%s|\n",lne(nf),b0);
								fprintf (hfaux,"%s","\n");
							}
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */
}


#endif


/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 7
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 7
 *
 *	lee archivo "check" (salida de grep de variables en codebase )
 *	y formatea para que se vea lindo
 */


int	pro_exec7()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	int	f_proceso;
	int	f_sigo;
	int	p1,p2;
	char	b1[MAXB];
	char	b2[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec7");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout )
		gp_uso(105);



	/* cantidad de archivos y lineas totales cargadas  */
	ql = 0;

	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		/* saco el fin de linea - contemplo 13 x fuentes fortran */
		for ( flag=0, j=strlen(b1); !flag && j >= 0; j--)
			if (b1[j] == '\n' )
			{	
				flag=1;
				if ( j && b1[j-1] == 13)
					b1[j-1]=0;
				else
					b1[j]=0;
			}

		f_proceso = 1;
		if (linea_vacia(b1) || b1[0] == '#' )
			f_proceso = 0;


		if (f_proceso)
		{

			for (j=0, f_sigo=1; f_sigo && j < strlen(b1); j++)
				if (b1[j]==':')
					p1 = j, f_sigo = 0;

			memset(b2,' ',30);
			b2[30]=0;

			strncpy(b2,b1,p1);
			strcat(b2,"|");
			strcat(b2,b1+p1+1);
			strcat(b2,"|");

			fprintf (hfout,"%s\n",b2);


		}

	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	f_name
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	devuelve nombre de file en funcion de la linea de codigo nl
 *
 */
 

char	*f_name(nl)
int	nl;
{
	static char b1[MAXV];

	int	i,j,k;
	int	f1;
	int	pf,uf;
	
	for (j=0, f1=1; f1 && j < qf_ff; j++)
	{
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

		if (pf <= nl && nl <= uf)
		{
			strcpy (b1, (*tb[j]).n );
			f1 = 0;
		}
	}

	return (b1);
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	n_name
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	devuelve subindice en tb de file en funcion de la linea de codigo nl
 *
 */
 
int	n_name(nl)
int	nl;
{
	int	i,j,k;
	int	pf,uf;
	int	f1;
	int	f_res;
	int	n_f;


	i=nl;

	for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
		if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
		{	n_f = j;
			pf = (*tb[j]).pf;
			uf = (*tb[j]).uf;
			f1=0;
		}

	return (n_f);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	b_name   
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	devuelve nombre de file sin ext en funcion de la linea de codigo nl
 *	atencion !!!
 *	En el estado actual, estoy pasando en el call, el numero de file en tb
 *	y no la linea en fnp .... HAY QUE REVISAR ESO !!!
 *
 */
 

char	*b_name(nl)
int	nl;
{
	static char b1[MAXV];

	int	i,j,k;
	int	f1;
	int	pf,uf;
	int	l2;
	
	if (nl >= qf_ff)
		error(30002);


	strcpy(b1, (*tb[nl]).n );
	l2 = strlen(b1);
	for (k=0; k<l2; k++)
		if (b1[k] == '.')
			b1[k]=0;

#if 0
	for (j=0, f1=1; f1 && j < qf_ff; j++)
	{
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

		if (pf <= nl && nl <= uf)
		{
			strcpy (b1, (*tb[j]).n );
			l2 = strlen(b1);
			for (k=0; k<l2; k++)
				if (b1[k] == '.')
					b1[k]=0;
			f1 = 0;
		}
	}
#endif

	return (b1);
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	e_name
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	devuelve extension de file en funcion de la linea de codigo nl
 *
 * 	tipo_ext
 *	0  no lo encontro
 *	1  for
 *	2  f90
 */
 

char	*e_name(nl)
int	nl;
{
	static char b1[MAXV];
	static char b2[MAXV];
	char        b3[MAXB];

	int	i,j,k;
	int	l2;
	int	f1;
	int	pf,uf;
	int	f_sig;
	int	tipo_ext;

	char	z[MAXV];
	int	w;
	sprintf (z,"e_name");
	w = g_dbg(z);

	
	for (j=0, f1=1; f1 && j < qf_ff; j++)
	{
		pf =  (*tb[j]).pf;
		uf =  (*tb[j]).uf;

if (gp_debug && w )
{
mprintf (z,"nl |%d| \n",nl);
mprintf (z,"j  |%d| \n",j);
mprintf (z,"pf |%d| \n",pf);
mprintf (z,"uf |%d| \n",uf);
mprintf (z,"n  |%s| \n", (*tb[j]).n );
}


		if (pf <= nl && nl <= uf)
		{
			strcpy (b1, (*tb[j]).n );
			f1 = 0;
if (gp_debug && w )
{
mprintf (z,"if |%s| \n", b1 );
}
		}
	}

	strcpy (b3,pasar_a_minusc(b1));

if (gp_debug && w )
{
mprintf (z,"e_name: b1 |%s| \n",b1);
mprintf (z,"e_name: b3 |%s| \n",b3);
}

	strcpy (b2,"ntf");
	tipo_ext = 0;
	f_sig = 1;
	l2 = strlen(b3);

	if (f_sig)
	{
		for (i=0; i<l2; i++)
			if (!strncmp(b3+i,".for",4))
			{
				strcpy(b2,"for");
				f_sig = 0;
			}
	}

	if (f_sig)
	{
		for (i=0; i<l2; i++)
			if (!strncmp(b3+i,".f90",4))
			{
				strcpy(b2,"f90");
				f_sig = 0;
			}
	}

		
if (gp_debug && w )
{
mprintf (z,"b2 |%s| \n",b2);
mprintf (z,"--- \n");
}

	return (b2);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	complejidad_mon
 *
 * -----------------------------------------------------------------------------------
 */


int	complejidad_mon(pf,uf,m_nl,m_nc)
int	pf;
int	uf;
int	*m_nl;
int	*m_nc;
{
	int	i,j,k;
	int	f_sig;
	int	f1,f2,f3;
	int	ql,qc;
	char	b1[MAXB];

	ql = 0;
	qc = 0;

	for (i = pf; i <= uf; i++)
	{
		strcpy(b1,pasar_a_minusc((*fnp[i]).l)  );

		if (!es_linea_comentario(b1))
		{
			ql++;

			for (j=0, f_sig = 1; f_sig && j < strlen(b1); j++)
			{	
				f3 = strncmp("common",b1+j,6);
printf ("complej: %5d f3: %d |%s| \n",j,f3,b1);

				if (!f3 && ( b1[j+6] == '/' || b1[j+7] == '/' || b1[j+8] == '/')  )
				{
		printf ("complejidad: BINGO !!! \n");

					f_sig = 0, qc ++;
				}
			}
		}
	}

	*m_nl = ql;
	*m_nc = qc;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	es_mon
 *
 * -----------------------------------------------------------------------------------
 */


int	es_mon(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2;
	int	f_sig,f_ret;
	char	b1[MAXB];


	f_ret = 0;
	strcpy (b1,pasar_a_minusc(s));


	for (i=0, f_sig = 1; f_sig && i < strlen(s); i++)
		if (!strcmp(".mon",b1+i))
			f_ret = 1, f_sig = 0;

	return ( f_ret );
}
	


/*
 * -----------------------------------------------------------------------------------
 *
 *	info_cmp1
 *
 * -----------------------------------------------------------------------------------
 */

#if 0
int info_cmp1(const void *p1, const void *p2) 
{
    // Convertir punteros void a punteros char
    const char *a = (const char *)p1;
    const char *b = (const char *)p2;

    return strcmp(a, b);
}
#endif


int	info_cmp1(p1,p2)
const	void	*p1;
const	void	*p2;
{
	int k;

	k = strcmp((const char *)p1,(const char *)p2);

	if (gp_fverbose("d4"))
	{
		printf ("SSS info_cmp %s %s %d !! \n",p1,p2,k);
	}

	return k;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	info_cmp2
 *
 * -----------------------------------------------------------------------------------
 */


int	info_cmp2(p1,p2)
const	void	*p1;
const	void	*p2;
{
	int k;

	k = strcmp((const char *)p1,(const char *)p2);

	if (gp_fverbose("d4"))
	{
		printf ("SSS info_cmp %s %s %d !! \n",p1,p2,k);
	}

	return k;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex3_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec3
 * hace algo con todas las lineas cargadas en memoria
 *
 */

int	ex3_p1()
{
	int 	i,j;
	int	f1;
	int	n_f;
	char	b1[MAXB];
	char	b2[MAXB];



	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
				n_f = j, f1=0;

		strcpy(b1,(*fnp[i]).l );

		if (!es_linea_comentario(b1) && tiene_include_v4(b1,b2))
		{
printf ("INC encontrado: %s \n",b2);

			fprintf (hflog,"%-30.30s  %s\n", (*tb[n_f]).n, b2);
			fprintf (hfout,"%-15.15s  %s\n", b2, (*tb[n_f]).n);
		}


#if 0
		strcpy(b1,(*fnp[i]).l );
		if ( es_linea_comentario(b1) )
		{
			strcpy(verif,analisis_comentario(b1));
			fprintf (hflog,"%s|%s|\n",verif,b1);
			fprintf (hflog,"%s\n",b1);
		}
#endif
	}

}


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_include_v4
 *
 * -----------------------------------------------------------------------------------
 */


bool	tiene_include_v4(s,t) 
char	*s;
char	*t;
{
	// Define la cadena a buscar
	const 	char *keyword = "include";
	char	b1[MAXB];

	bool	r;
	strcpy	(b1,pasar_a_minusc(s));
	char 	*start = strstr(b1, keyword);


	r = false;

	if (start != NULL) 
	{
		// Avanza el puntero más allá de "include"
		start += strlen(keyword);

		// Salta los espacios en blanco
		while (isspace(*start)) 
		{
		    start++;
		}

		// Verifica si el siguiente carácter es un apóstrofe
		if (*start == '\'') 
		{
		    // Salta el apóstrofe inicial
		    start++; 

		    // Encuentra el apóstrofe final
		    char *end = strchr(start, '\'');
		    if (end != NULL) 
		    {
			// Copia el nombre de archivo sin las comillas en t
			strncpy(t, start, end - start);
			t[end - start] = '\0';
			r =  true;
		    }
		}
	}

	// resultado
	return r;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	primer_char
 *
 * -----------------------------------------------------------------------------------
 */



#if 0
#include <stdio.h>
#include <ctype.h>
#endif

int primer_char(char *s, int *p)
{
    int pos = 0;
    while (s[pos])
    {
	if (!isspace(s[pos]))
	{
	    *p = pos;
	    return 1;
	}
	pos++;
    }
    return 0;
}

#if 0
int main()
{
    char s[] = "    abc 123";
    int p;

    int resultado = primer_char(s, &p);

    if (resultado)
    {
	printf("La posición del primer carácter distinto de blanco es: %d\n", p);
    }
    else
    {
	printf("No se encontró ningún carácter distinto de blanco.\n");
    }

    return 0;
}
#endif










/*
 * -----------------------------------------------------------------------------------
 *
 *	es_nombre_valido2
 *
 * -----------------------------------------------------------------------------------
 */


#if 0
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#endif


// Función para verificar si una cadena es un nombre válido en Fortran
int es_nombre_valido2(const char *str)
{
    if (!isalpha(*str)) // El primer carácter debe ser una letra
	return 0;
    
    while (*str && *str != '_')
    {
	if (!isalnum(*str))
	    return 0;
	str++;
    }
    
    // Si encontramos un '_', verificar la siguiente parte
    while (*str)
    {
	if (*str == '_')
	{
	    str++;
	    if (!isalnum(*str)) // Después de '_' debe haber al menos una letra o número
		return 0;
	}
	else if (!isalnum(*str))
	{
	    return 0;
	}
	str++;
    }

    return 1;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_call_a_entry
 *
 * -----------------------------------------------------------------------------------
 */


// Función principal para verificar si se encuentra "call nombre_valido" en la cadena s
int tiene_call_a_entry(char *s, char *t)
{
    char b1[MAXB];

    strcpy(b1,pasar_a_minusc(s));

    char *ptr = b1;
    char *call_pos = strstr(ptr, "call");

    if (!call_pos)
	return 0;

    ptr = call_pos + 4; // Avanzar después de "call"
    while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
	ptr++;

    // Verificar si el nombre válido después de "call" coincide con el nombre válido en t
    if (strncmp(ptr, t, strlen(t)) == 0)
    {
	ptr += strlen(t);
	while (*ptr && isspace(*ptr)) // Saltar espacios en blanco después de nombre_valido
	    ptr++;
	
	if (*ptr == '\0' || *ptr == '(' || *ptr == '!') // Asegurarse de que no hay caracteres extra después del nombre válido
	    return 1;
    }

    return 0;
}

#if 0
int main()
{
    char s[] = "call   str1_str2_str3  ";
    char t[] = "str1_str2_str3";

    int resultado = tiene_call_a_entry(s, t);

    if (resultado)
    {
	printf("Se encontró la expresión 'call %s' en la línea.\n", t);
    }
    else
    {
	printf("No se encontró la expresión 'call %s' en la línea.\n", t);
    }

    return 0;
}
#endif





/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_funcion_a_entry
 *
 * -----------------------------------------------------------------------------------
 */



// Subrutina que verifica si "nombre_valido" es invocada como función en la línea
int tiene_funcion_a_entry(char *linea, char *nombre_valido) {
    char b1[MAXB];
    strcpy(b1, pasar_a_minusc(linea));

    char *ptr = b1;
    char *func_pos = strstr(ptr, nombre_valido);

    while (func_pos) {
	ptr = func_pos + strlen(nombre_valido);

	// Saltar espacios en blanco después del nombre de la función
	while (*ptr && isspace(*ptr))
	    ptr++;

	// Verificar si está seguido de un paréntesis de apertura
	if (*ptr == '(') {
	    ptr++; // Avanzar después de "("
	    int paren_count = 1;

	    // Verificar el contenido dentro de los paréntesis
	    while (*ptr && paren_count > 0) {
		if (*ptr == '(')
		    paren_count++;
		else if (*ptr == ')')
		    paren_count--;
		ptr++;
	    }

	    // Si se cerraron todos los paréntesis, es una invocación válida
	    if (paren_count == 0)
		return 1;
	}

	// Buscar la próxima aparición de nombre_valido
	func_pos = strstr(func_pos + 1, nombre_valido);
    }

    return 0; // No se encontró una invocación válida
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_entry
 *
 * -----------------------------------------------------------------------------------
 */



int tiene_entry(const char *linea) 
{
    const char *ptr = linea;
    int	f1;


    // Convertir a minúsculas para hacer la búsqueda insensible a mayúsculas
    char buffer[MAXB];
    int i = 0;

    while (*ptr && i < MAXB) 
    {
	buffer[i++] = tolower(*ptr++);
    }
    buffer[i] = '\0';

    // Buscar "entry" en el string
    char *entry_pos = strstr(buffer, "entry");
    while (entry_pos) 
    {
	// Verificar que "entry" no esté precedido ni seguido por caracteres alfanuméricos o guiones bajos
	if ((entry_pos == buffer || !isalnum(*(entry_pos - 1)) && *(entry_pos - 1) != '_') &&
	    isspace(*(entry_pos + 5))) 
	{
	    return TRUE;
	}
	// Buscar la próxima aparición de "entry"
	entry_pos = strstr(entry_pos + 1, "entry");
    }

    return FALSE;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	primer_caracter_es_mas
 *
 * -----------------------------------------------------------------------------------
 */


int	primer_caracter_es_mas(s)
char	*s;
{
	int	i,j,k;
	int	f_res, f_cont;

	f_res = 0;


	/* caso     "+   entry at ...." */
	if (!f_res)
	{
		for (i=0, f_cont=1; f_cont && i<strlen(s); i++)
			if (s[i] == '+')
				f_res=1, f_cont=0;
	}

	return f_res;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_entry_v1
 *
 * -----------------------------------------------------------------------------------
 */


#if 0
int	tiene_entry_v1(s,t)
char	*s;
char	*t;
{





}
#endif



#if 0
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#endif



/*
 * -----------------------------------------------------------------------------------
 *
 *	es_nombre_valido
 *
 * -----------------------------------------------------------------------------------
 */


// Función para verificar si una cadena es un nombre válido en Fortran
int es_nombre_valido(const char *str)
{
    if (!isalpha(*str)) // El primer carácter debe ser una letra
	return 0;
    
    while (*str && *str != '_')
    {
	if (!isalnum(*str))
	    return 0;
	str++;
    }
    
    // Si encontramos un '_', verificar la siguiente parte
    while (*str)
    {
	if (*str == '_')
	{
	    str++;
	    if (!isalnum(*str)) // Después de '_' debe haber al menos una letra o número
		return 0;
	}
	else if (!isalnum(*str))
	{
	    return 0;
	}
	str++;
    }

    return 1;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_entry_v1
 *
 * -----------------------------------------------------------------------------------
 */

// Función principal para buscar la expresión
int tiene_entry_v1(char *s, char *t)
{
    char b1[MAXB];
    strcpy(b1,pasar_a_minusc(s));


    char *ptr = b1;
    char *entry_pos = strstr(ptr, "entry");

    if (!entry_pos)
	return 0;

    ptr = entry_pos + 5; // Avanzar después de "entry"
    while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
	ptr++;

    char nombre_valido[256];
    int i = 0;

    while (*ptr && (isalnum(*ptr) || *ptr == '_'))
    {
	nombre_valido[i++] = *ptr++;
    }

    nombre_valido[i] = '\0';

    if (!es_nombre_valido(nombre_valido))
	return 0;

    while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
	ptr++;

    if (*ptr == '(')
    {
	ptr++;
	while (*ptr && isspace(*ptr)) // Saltar espacios en blanco
	    ptr++;
    }

    if (strstr(ptr, "return"))
	return 0;

    strcpy(t, nombre_valido);
    return 1;
}


#if 0
int main()
{
    char s[] = "entry   str1_str2_str3  ";
    char t[256];

    int resultado = tiene_entry_v1(s, t);

    if (resultado)
    {
	printf("Se encontró la expresión. Nombre válido: %s\n", t);
    }
    else
    {
	printf("No se encontró la expresión o contiene 'return'.\n");
    }

    return 0;
}
#endif











#if 0

bool	tiene_include_v3(s,t) 
char	*s;
char	*t;
{
	// Define la cadena a buscar
	const 	char *pattern = "   include  '";
	int 	l1;
	char	b1[MAXB];
	bool	r;

	r = false;
	l1 = strlen(pattern);
	strcpy (b1,pasar_a_minusc(s));

printf ("TIENE1:  r: %d  b1: |%s| \n",r,b1);


	// Busca el patrón en el string 
	char *start = strstr(b1, pattern);

	if (start != NULL) 
	{
		// Encuentra el inicio del nombre de archivo
		start += l1;
		char *end = strchr(start, '\'');
	
		if (end != NULL) 
		{
			// Copia el nombre de archivo sin las comillas en t
			strncpy(t, start, end - start);
			t[end - start] = '\0';
			r = true;
		}
	}

printf ("TIENE2:  r: %d  b1: |%s| \n",r,b1);

	// resultado
	return r;
}
#endif



#if 0

int	tiene_include_v2(s,t)
char	*s;
char	*t;
{
	int	i,j,k;
	int	p1,p2,p3;
	int	f1,f2,f3,f_sig,f_go;
	int	m1,m2;
	int	f_res;
	char	b1[MAXB];
	char	b2[MAXB];

	f_res = 0;
	f_sig = 1;

	/* tiene la palabra include ? */
	if (f_sig)
	{
		f_sig = 0;

		for (i=0 , f_go = 1;  f_go && i < strlen(s); i++)
			if (!strcmp("include", pasar_a_minusc(s+i)))
				p1 = i+7, f3 = 1, f2 = 0;

	}

	return (f_res);
}

#endif

/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_include
 *
 * -----------------------------------------------------------------------------------
 */

#if 0

int	tiene_include(s,t)
char	*s,*t;
{

	int	i,j,k;

	f1 = 0;

	/* tiene la palabra include ... ? */
	for (i=0 , f2=1, f3=0; f2 && i < strlen(s); i++)
		if (!strcmp("include", pasar_a_minusc(s+i)))
			p1 = i+7, f3 = 1, f2 = 0;

	/* despues de include, solo puede haber blancos y un ' */
	if (f3)
	{	
		for(i=p1, f2=1, f4=0; f2 && i < strlen(s); i++)
			
	

	s_count = 0;

	p1 = 0;
	f1 = 1;
	f2 = 1;
	f3 = 1;
	l1 = 0;
	i  = 0;


	while ( f2 )
	{
		c = b1[p1 + i];
		k = 0;
			
		if ( c >= 'a' && c <= 'z' )
			k = 1;

		if ( c >= 'A' && c <= 'Z' )
			k = 1;

		if ( c >= '0' && c <= '9' )
			k = 2;

		if ( c == ' ' )
			k = 3;

		if ( c == '_' )
			k = 4;

		if ( c == '.' )
			k = 5;

		if ( c == '/' )
			k = 6;

printf ("while: i: %d f9: %d k/c: %c%c |%s| \n",i,f9,c,b1[1+i+1],s);

		switch (k)
		{
			case 	6:
				if ( !f9 && i == 0 )
					f9 = 1, i++;

				if ( !f9 && i )
					i++;

				if ( f9 && i > 0 )
					f2 = 0;

				break;

			default:
				if ( c == 0 )
					f2 = 0;
				i++;
				break;
					

#if 0
				/* no es caracter valido en la cadena, terminamos */
			case	0:
				f2 = 0;
				if (f1 == 0)
					f9 = i;
				else
					f9 = 0;
				break;

				/* es letra */
			case	1:
				if (f1)
					f1=0;
				i++;
				break;

				/* es numero , solo si hubo una letra antes */
			case	2:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;


				/* aceptamos casos en los que hay blancos como parte del nombre !! */
			case 	3:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;

				/* aceptamos casos en los que hay _ como parte del nombre !! */
			case 	4:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;
#endif
		}			
	}




}


#endif













		



/*
 * -----------------------------------------------------------------------------------
 *
 *	analisis comentario
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * analiza un comentario para preparar entrenamiento de IA
 *
 * 00 - no hay certeza de lo que es 
 * 01 - es un comentario
 * 05 - es una linea de codigo
 * 
 */


char	*analisis_comentario(s)
char	*s;
{
	static char	verif[8];
	int	i,j,k,f1,f2;
	int	l1,l2,l3,l4;


	memset(verif,0,8);

	strcpy(verif,"00");
	f1 = 0;

	/* lineas tipo rayas, separadores etc */
	if (!f1)
	{
		for (i=0, l1=0, l2=0, l3=0, l4=0; i < strlen(s); i++)
		{		
			if (s[i] == 'C')
				l1++;
			if (s[i] == 'c')
				l2++;
			if (s[i] == '!')
				l3++;
			if (s[i] == '*')
				l4++;
		}

		if (l1 > 20 || l2 > 20 || l3 > 20 || l4 > 20 )
			f1 = 1;
	}

	/* lineas vacias */
	if (!f1)
	{
		if (strlen(s) < 4)
			f1 = 1;
	}



	sprintf (verif,"%2d",f1);
	
	return (verif);
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 3
 *
 *	exec aparte ...
 *
 * -----------------------------------------------------------------------------------
 */



#if 0

int	pro_exec3()
{

	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a proceso exec 3 \n\n",gp_tm());
	}
		


	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de proceso exec 3 \n\n",gp_tm());
	}

}

#endif





/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 8
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 8
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 *
 *	proceso para hacer mapa de variables de todo el codebase
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec8()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"exec8");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp)
		gp_uso(115);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	if (gp_suboption == 1 )
	{
		/* sub ocpcion 1 -opt=1 */
		ex8_p1();
	}


	if (gp_suboption == 2 )
	{
		/* sub opcion 2 -opt=2 */
		ex8_p2();
	}

	if (gp_suboption == 3 )
	{
		/* sub opcion 3 -opt=3 */
		ex8_p3();
	}


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
		{
			printf ("No puedo abrir: |%s| \n",d2);
			error(605);
		}

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex8_p1
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec8
 * hace algo con todas las lineas cargadas en memoria
 *
 *
 */

int	ex8_p1()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;
	int	n_var;


	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		/* trabajemos con minusculas */
		strcpy (b2, pasar_a_minusc(b1));


		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);


		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			v_pars(b2,&n_var,v_size,v_name );

			for (i=0; i<n_var; i++)
			{
				memset(b4,0,sizeof(b4));
				strncpy(b4,v_name[i],v_size[i]);
				fprintf (hfout,"%30.30s %2d |%s| \n",prog_name,i,b4);
			}



#if 0
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

#if 0
							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;

							}
#endif
						}
					}
				}		
	
			} /* while f5 */
#endif

		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



}







/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_exec 9
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	exec 9
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 *
 *	utilizado para analizar / modificar sentencias allocate
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_exec9()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	int	x1;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	int	w;
	sprintf (z,"pro_exec9");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffaux )
		gp_uso(105);
	if (!ffsta)
		agregar_ffsta("tool.sta");


	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
		printf ("\n");
	}

#if 0
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}

		printf ("\n");
	}
#endif
	printf ("\n");


	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

	if (gp_suboption == 1 )
	{
#if 0
		/* repasa archivos y corrige lineas que tienen chequeos dist de check_alloc */
		ex9_p1_a();
#endif

		/* busca allocate en todos los programas    */
		ex9_p1_b();

#if 1
		if (gp_proceed)
		{

			/* desdoabla lineas largas en los progs     */
			/*
			 * solo si puse gp_proceed => estoy agregando check_allocs
			 * esta habilitado y parece que funcionando bien para f90
			 * faltaria para for ...
			 *
			 */

			ex9_p1_c();
		}
#endif
	}

	if (gp_suboption == 2 )
	{
		/* prueba de mover todas las lineas         */
		ex9_p2();
	}

	if (gp_suboption == 3 )
	{
		/* prueba de mover todas las lineas         */
		ex9_p3();
	}

	if (gp_suboption == 4 )
	{
		/* pba de comb hacer_lugar con mover_bulk   */
		ex9_p4();
	}




if (gp_debug && w)
{
mprintf (z,"Voy a gragar files modificados \n");
mprintf (z,"qf_ff: %d \n",qf_ff);
for (i=0; i<qf_ff; i++)
{
mprintf (z,"tb[i]:  %d  %d  %d  |%s| \n",i,(*tb[i]).pf,(*tb[i]).uf,(*tb[i]).n);
}
for (j=0; j<qf_ff; j++)
{
for (i= (*tb[j]).pf ; i<= (*tb[j]).uf; i++)
{
mprintf (z,"%3d |%s|\n",i,(*fnp[i]).l);
}
}

mprintf (z,"---- \n");
}

	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(607);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	mostrar_reportes("exec9");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	ex9_p1_a
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec9
 * hace algo con todas las lineas cargadas en memoria
 *
 * detecta si tiene sentencia allocate
 * revisa si esta check_alloc bien armado, sino, 
 * vuelve la sentencia a formato original, para 
 * ex9_p1_b corrija.
 *
 */

#if 0
int	ex9_p1_a()
{

	return (1);
}
#endif


/*
 * files con algo distinto al check_alloc convencional
 * en cada caso hay que hacer algo ...
 *

andecomp.f90
clm_objt.f90
cn_objt.f90
el_objt.f90
facalg.f90
fe_objt.f90
fuelused.f90
gas_objt.for
lpsolver.f90
msgoutpt.for
servicac.f90

 *
 */






/* ex9_p1_a */
#if 1

int	ex9_p1_a()
{
	static	int	f_miscmod1 = 0;
	static	int	f_miscmod2 = 0;
	static	int	f_msgmmsb6 = 0;

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4;
	int	f1,f2,f3,f4,f5;
	int	p1,p2,p3,p4;
	int	f_proceso;
	int	f_stat;
	int	f_keep;
	int	f_act;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	b7[MAXB];
	int	pf,uf,nf;
	int	tipo_ext;
	int	add_lines;		/* cant de lineas que hay que sumar al fuente al corregir un allocate */
	int	num_alloc;		/* numero de alloc encontrado en src */
	int	num_alloc_key;		/* numero de key usado en file para identificar alloc en fuente */
	int	num_alloc_fnd;		/* num de alloc cuando es encontrado en src */
	int	nf_check;		/* linea donde esta el check alloc encontrado en el fuente */
	int	r1;
	int	k_amp;
	int	n1,n2;
	
	int	es_tipo,tipo[20];

	int	tipo01;
	int	tipo02;
	int	tipo03;
	int	tipo04;
	int	tipo05;
	int	tipo06;
	int	tipo07;
	int	tipo08;
	int	tipo09;
	int	tipo11;
	int	tipo13;
	int	tipo15;
	int	tipo16;

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"ex9_p1_a");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* stats */
	sq_q_alloc         = 0;
	sq_q_alloc_for_sta = 0;
	sq_q_alloc_f90_sta = 0;

	sq_q_alloc_for_slc = 0;
	sq_q_alloc_for_clc = 0;
	sq_q_alloc_for_sav = 0;
	sq_q_alloc_for_mav = 0;
	sq_q_alloc_for_slc_sav = 0;		/* cant tot de lineas sin cont con allocate single var */
	sq_q_alloc_for_clc_sav = 0;		/* cant tot de lineas con cont con allocate single var */
	sq_q_alloc_for_slc_mav = 0;		/* cant tot de lineas sin cont con allocate multi var */
	sq_q_alloc_for_clc_mav = 0;		/* cant tot de lineas con cont con allocate multi var */

	sq_q_alloc_f90_slc = 0;
	sq_q_alloc_f90_clc = 0;
	sq_q_alloc_f90_sav = 0;			/* cantidad total de lineas allocate con single var */
	sq_q_alloc_f90_mav = 0;			/* cantidad total de lineas allocate con multiple var */
	sq_q_alloc_f90_slc_sav = 0;		/* cant tot de lineas sin cont con allocate single var */
	sq_q_alloc_f90_clc_sav = 0;		/* cant tot de lineas con cont con allocate single var */
	sq_q_alloc_f90_slc_mav = 0;		/* cant tot de lineas sin cont con allocate multi var */
	sq_q_alloc_f90_clc_mav = 0;		/* cant tot de lineas con cont con allocate multi var */


	/* para todas las lineas */
	
	tipo01 = 0;
	tipo03 = 0;
	tipo05 = 0;
	tipo07 = 0;

	tipo09 = 0;
	tipo11 = 0;
	tipo13 = 0;
	tipo15 = 0;

	for (j=0; j<20; j++)
		tipo[j]=0;

	i = 0;
	f_keep = 1;
	f_act  = 0;

	while (f_keep)
	{
		strcpy(b0,(*fnp[i]).l );

if (gp_debug && w)
{
	mprintf (z,"FOR prin (%3d) |%s| \n",i,b0);
	mprintf (z,"... \n\n");
}


		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));
		strcpy(exte_name,e_name(i));

		if (!strncmp(exte_name,"for",3))
			tipo_ext = 1;

		if (!strncmp(exte_name,"f90",3))
			tipo_ext = 2;

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
			num_alloc = 0;
			num_alloc_key = 1;

if (1)
{
printf ("Trabajo con ... (%s) |%s|\n",exte_name,prog_name);
}

		}


		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1, pasar_a_minusc(b0));
		strcpy(b2, pasar_a_minusc(b0));


		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;


		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			if (tiene_allocate(b1))
			{

if (gp_debug && w)
{
	mprintf (z,"allocate: |%s| \n",b1);
}

				num_alloc++;
				sq_q_alloc++;

				c2++;

				fprintf (hfout,"%05d %-30.30s (%s) %05d |%s|\n",
					c2,
					prog_name,
					exte_name,
					i-pf+1,
					b0);

				if (tipo_ext == 1)
					fprintf (hfaux,"%s\n",b0);

				if (tipo_ext == 2)
					fprintf (hflog,"%s\n",b0);

				/* es .for EFOR ------------------------------------  */
				if(tipo_ext == 1 ) 
				{
					c3++;

					if (!tiene_mas( (*fnp[i+1]).l ) )
					{
						/* no tiene lineas de continuacion ... */
						sq_q_alloc_for_slc++;
	
						strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(b1))  
						{
							/* Es un allocate en una linea con una sola variable */
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_slc_sav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{
								/* TIPO-1 alloc - una linea - una sola variable - sin stat */
								es_tipo=1;
								tipo[es_tipo]++;

								tipo01++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan_rf(1,1,1,0,0,prog_name,m1);

#if 0
								f_act = 1;
								if (gp_proceed )
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,1,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}
if (gp_debug && w)
{
mprintf (z,"TIPO-1 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;

								}
#endif

							}
							else
							{
								/* TIPO-2 alloc - una linea - una sola variable - con stat */
								es_tipo=2;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(b1));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));

									grabar_plan_rf(2,1,1,1,1,prog_name,m1);

									grabar_mapa_rf(1,2,prog_name,0,0,0);     
									grabar_mapa_rf(6,0," ",num_alloc,0,0);
									grabar_mapa_rf(8,2,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa_rf(0,0,"-",0,0,2);

								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan_rf(2,1,1,1,2,prog_name,m1);
								}


							}
						}
						else /* if (!tiene_multiple ... ) */
						{
							/* Es un allocate en una linea con una varias variables */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_slc_mav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{
								/* TIPO-3 es un alloc en una linea con mas de una var - sin stat */
								es_tipo=3;
								tipo[es_tipo]++;

								tipo03++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan_rf(3,1,2,0,0,prog_name,m1);

#if 0
								f_act = 1;

								if (gp_proceed )
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,3,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}
if (gp_debug && w)
{
mprintf (z,"TIPO-3 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g03(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}
#endif
							}	
							else
							{
								/* TIPO-4 alloc - una linea - una sola variable - con stat */
								es_tipo=4;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan_rf(4,1,2,1,0,prog_name,m1);

							}
						}
					}
					else
					{
						/* tiene lineas de continuacion ... */
						sq_q_alloc_for_clc++;
						k=0;
						memset(m0,0,MSTR);

						do
						{

							if (!es_linea_comentario( (*fnp[i+k]).l ))
							{
								strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
								strcpy(b1, trim_beg(b1));
								strcpy(b1, trim_end(b1));
								strcpy(b1, trim_blanks(b1));
								strcat(m0,b1);
							}
							k++;
						}
						while ( tiene_mas(  (*fnp[i+k]).l ) || 
							(es_linea_comentario( (*fnp[i+k]).l) && tiene_mas( (*fnp[i+k+1]).l))   );
					
if (gp_debug && w)
{
mprintf (z,"alloc: 1 m0: |%s|\n",m0);
}
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(m0))
						{
							/*  tiene lineas de cont ... solo una variable   */ 
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_clc_sav++;

							if (!f_stat)
							{
								/* TIPO-5 es un alloc en varias lineas con una sola variable - sin stat  */
								es_tipo=5;
								tipo[es_tipo]++;

								tipo05++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan_rf(5,2,1,0,0,prog_name,m1);


#if 0
								if (gp_proceed)
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,5,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

if (gp_debug && w)
{
mprintf (z,"TIPO-5 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g05(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}
#endif

							}
							else
							{
								/* TIPO-6 es un alloc en varias lineas con una sola variable - con stat  */
								es_tipo=6;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(m0));


								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan_rf(6,2,1,1,1,prog_name,m1);

									grabar_mapa_rf(1,6,prog_name,0,0,0);     
									grabar_mapa_rf(6,0," ",num_alloc,0,0);
									grabar_mapa_rf(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa_rf(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan_rf(6,2,1,1,2,prog_name,m1);
								}



							}
						}
						else
						{
							/* es un alloc con lineas de cont y varias vars */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_clc_mav++;

							if (!f_stat)
							{
								/* TIPO-7 es un alloc en varias lineas con varias variables - sin stat */
								es_tipo=7;
								tipo[es_tipo]++;

								tipo07++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan_rf(7,2,2,0,0,prog_name,m1);

#if 0
								if (gp_proceed )
								{
									flag_alloc_ok = 0;
									grabar_mapa_rf(1,7,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

if (gp_debug && w)
{
mprintf (z,"g7_1, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 


									/* agrego el check_alloc */
									chg_alloc_g07(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}
#endif

							}
							else
							{
								/* TIPO-8 es un alloc en varias lineas con varias variables - con stat */
								es_tipo=8;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan_rf(8,2,2,0,0,prog_name,m1);
							}
						}
					}
				}


				/* es .f90 EF90 ------------------------------------  */
				if(tipo_ext == 2)
				{

					c4++;

					if (!tiene_amper( (*fnp[i]).l ) )
					{
						/* es un allocate sin linea de continuacion */
						sq_q_alloc_f90_slc++;

						strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
						f_stat = tiene_stat(b1);


						if (!tiene_multiple_vars(b1))  
						{
							/* es un allocate en una linea con una sola variable */
							sq_q_alloc_f90_sav++;
							sq_q_alloc_f90_slc_sav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{

								/* TIPO-9 alloc en una sola linea una sola variable sin stat */
								es_tipo=9;
								tipo[es_tipo]++;

								tipo09++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan_rf(9,1,1,0,0,prog_name,m1);

#if 0
								if (gp_proceed )
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,9,prog_name,0,0,0);

									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

if (gp_debug && w)
{
mprintf (z,"TIPO-9 desp de chg_alloc ... add_lines: %d\n",add_lines);
}
									/* atenti:
									 * chg_alloc_t09 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente
									 * aumenta la cantidad de lineas del fuente ...
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
									chg_alloc_g09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

#endif
							}
							else   /* if (!f_stat) ... */
							{
								/* TIPO-10 alloc en una sola linea una sola variable con stat */
								es_tipo=10;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;

								strcpy(m1,trim_blanks_beg(b1));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan_rf(10,1,1,1,1,prog_name,m1);

									grabar_mapa_rf(1,10,prog_name,0,0,0);     
									grabar_mapa_rf(6,0," ",num_alloc,0,0);
									grabar_mapa_rf(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa_rf(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan_rf(10,1,1,1,2,prog_name,m1);
								}

							}
						}
						else /* if (tiene_continuacion ... */
						{
							/* es un allocate en una linea con varias vars - sin stat */
							sq_q_alloc_f90_mav++;
							sq_q_alloc_f90_slc_mav++;

							if (!f_stat)
							{
								/* TIPO-11 alloc en una linea con varias vars - sin stat */
								es_tipo=11;
								tipo[es_tipo]++;

								tipo11++;
								strcpy(m1,trim_blanks_beg(b1));

								grabar_plan_rf(11,1,2,0,0,prog_name,m1);

#if 0
								if (gp_proceed)
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,11,prog_name,0,0,0);

									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);
if (gp_debug && w)
{
mprintf (z,"TIPO-11 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
									chg_alloc_g15(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

if (gp_debug && w)
{
mprintf (z,"g15_A, volvi de chg_alloc g15 add_lines: %d  \n",add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
if (gp_debug && w)
{
mprintf (z,"g15_2, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 

										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}
#endif
							}
							else
							{

								/* TIPO-12 alloc en una sola linea con varias vars - con stat */
								es_tipo=12;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(b1));

								grabar_plan_rf(12,1,2,1,0,prog_name,m1);
							}
						}
					}
					else
					{

						/* es un allocate con linea de continuacion */

						sq_q_alloc_f90_clc++;
						k=0;
						memset(m0,0,MSTR);

						while ( tiene_amper(  (*fnp[i+k]).l ) ||
							(es_linea_comentario ( (*fnp[i+k]).l ) && tiene_amper( (*fnp[i+k+1]).l ))  )
						{

							if (!es_linea_comentario( (*fnp[i+k]).l ) )
							{
								strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
								strcpy(b1, trim_beg_f90(b1));
								strcpy(b1, trim_end_f90(b1));
								strcpy(b1, trim_blanks(b1));
								strcat(m0,b1);
								strcpy(m0,chanchada(m0));
							}
							k++;
						}

						strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
						strcpy(b1, trim_beg_f90(b1));
						strcpy(b1, trim_end_f90(b1));
						strcpy(b1, trim_blanks(b1));
						strcat(m0,b1);

					
						/* verifico si tiene STAT= */
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(m0))
						{
							sq_q_alloc_f90_sav++;
							sq_q_alloc_f90_clc_sav++;


							if (!f_stat)
							{
								/* TIPO-13 es un alloc en varias lineas con una var - sin stat */
								es_tipo=13;
								tipo[es_tipo]++;

								tipo13++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan_rf(13,2,1,0,0,prog_name,m1);

#if 0
								if (gp_proceed)
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,13,prog_name,0,0,0);
									chg_alloc_t13(n_f,i,&add_lines,num_alloc,f_act);

									/* atenti:
									 * chg_alloc_t13 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente
									 * aumenta la cantidad de lineas del fuente ...
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
if (gp_debug && w)
{
mprintf (z,"g02_1, entro a  chg_alloc_g02 num_alloc %d add_lines: %d  \n",num_alloc,add_lines);
} 
									chg_alloc_g02(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);
if (gp_debug && w)
{
mprintf (z,"g02_2, volvi de chg_alloc_g02 num_alloc %d add_lines: %d  \n",num_alloc,add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

#endif
							}
							else /* if (!f_stat ... ) */
							{
								/* TIPO-14 es un alloc en varias lineas con una var - con stat */
								es_tipo=14;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(m0));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan_rf(14,2,1,1,1,prog_name,m1);

									grabar_mapa_rf(1,14,prog_name,0,0,0);     
									grabar_mapa_rf(6,0," ",num_alloc,0,0);
									grabar_mapa_rf(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa_rf(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan_rf(14,2,1,1,2,prog_name,m1);
								}




							}
						}
						else /* if (!tiene_multiple_vars ... ) */
						{
							sq_q_alloc_f90_mav++;
							sq_q_alloc_f90_clc_mav++;

							
							if (!f_stat)
							{
								/* TIPO-15 es un alloc en varias lineas con varias vars - sin stat  */
								es_tipo=15;
								tipo[es_tipo]++;

								tipo15++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan_rf(15,2,2,0,0,prog_name,m1);

#if 0
								if (gp_proceed )
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,15,prog_name,0,0,0);

									/* atenti:
									 * chg_alloc_t15 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente, modificar lineas del fuente
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									chg_alloc_t15(n_f,i,&add_lines,num_alloc,f_act);


									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
if (gp_debug && w)
{
mprintf (z,"g15_1, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 
										i+= add_lines;
										flag_alloc_ok = 0;
									}


									/* agrego el check_alloc */
									chg_alloc_g15(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

if (gp_debug && w)
{
mprintf (z,"g15_2, volvi de chg_alloc g15 add_lines: %d  \n",add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{

										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}

#endif
							}
							else
							{
								/* TIPO-16 es un alloc en varias lineas con varias vars - con stat  */
								es_tipo=16;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(m0));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan_rf(16,2,2,1,1,prog_name,m1);

									grabar_mapa_rf(1,14,prog_name,0,0,0);     
									grabar_mapa_rf(6,0," ",num_alloc,0,0);
									grabar_mapa_rf(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa_rf(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan_rf(16,2,2,1,2,prog_name,m1);
								}

							}

						}
					}
				}
			}

		} /* proceso */

		/* atenti !!! que pasa con los multi lineas !! 
		 * ver cambios de variables ... i ?
		 */


		i++;
		if (i >= qf_src)
			f_keep = 0;

	} /* for .. while ... principal */




if (0)
{
	printf ("\n");
	printf ("tipo 01 %3d \n",tipo01);
	printf ("tipo 03 %3d \n",tipo03);
	printf ("tipo 05 %3d \n",tipo05);
	printf ("tipo 07 %3d \n",tipo07);

	printf ("tipo 09 %3d \n",tipo09);
	printf ("tipo 11 %3d \n",tipo11);
	printf ("tipo 13 %3d \n",tipo13);
	printf ("tipo 15 %3d \n",tipo15);
	printf ("\n");


	for (j=1; j<=16; j++)
	{
		printf ("tipo %2d:  %4d \n",j,tipo[j]);
	}
	printf ("\n");
}



}

#endif
/* ex9_p1_a */

/*
 * -----------------------------------------------------------------------------------
 *
 *	ex9_p1_b
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec9
 * hace algo con todas las lineas cargadas en memoria
 *
 * detecta si tiene sentencia allocate
 * devuelve true / false, y algunos parametros ...
 *
 */



int	ex9_p1_b()
{
	static	int	f_miscmod1 = 0;
	static	int	f_miscmod2 = 0;
	static	int	f_msgmmsb6 = 0;

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4;
	int	f1,f2,f3,f4,f5;
	int	p1,p2,p3,p4;
	int	f_proceso;
	int	f_stat;
	int	f_keep;
	int	f_act;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	b7[MAXB];
	int	pf,uf,nf;
	int	tipo_ext;
	int	add_lines;		/* cant de lineas que hay que sumar al fuente al corregir un allocate */
	int	num_alloc;		/* numero de alloc encontrado en src */
	int	num_alloc_key;		/* numero de key usado en file para identificar alloc en fuente */
	int	num_alloc_fnd;		/* num de alloc cuando es encontrado en src */
	int	nf_check;		/* linea donde esta el check alloc encontrado en el fuente */
	int	r1;
	int	k_amp;
	int	n1,n2;
	
	int	es_tipo,tipo[20];

	int	tipo01;
	int	tipo02;
	int	tipo03;
	int	tipo04;
	int	tipo05;
	int	tipo06;
	int	tipo07;
	int	tipo08;

	int	tipo09;
	int	tipo11;
	int	tipo13;
	int	tipo15;

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"ex9_p1_b");
	w = g_dbg(z);


	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* stats */
	sq_q_alloc         = 0;
	sq_q_alloc_for_sta = 0;
	sq_q_alloc_f90_sta = 0;

	sq_q_alloc_for_slc = 0;
	sq_q_alloc_for_clc = 0;
	sq_q_alloc_for_sav = 0;
	sq_q_alloc_for_mav = 0;
	sq_q_alloc_for_slc_sav = 0;		/* cant tot de lineas sin cont con allocate single var */
	sq_q_alloc_for_clc_sav = 0;		/* cant tot de lineas con cont con allocate single var */
	sq_q_alloc_for_slc_mav = 0;		/* cant tot de lineas sin cont con allocate multi var */
	sq_q_alloc_for_clc_mav = 0;		/* cant tot de lineas con cont con allocate multi var */

	sq_q_alloc_f90_slc = 0;
	sq_q_alloc_f90_clc = 0;
	sq_q_alloc_f90_sav = 0;			/* cantidad total de lineas allocate con single var */
	sq_q_alloc_f90_mav = 0;			/* cantidad total de lineas allocate con multiple var */
	sq_q_alloc_f90_slc_sav = 0;		/* cant tot de lineas sin cont con allocate single var */
	sq_q_alloc_f90_clc_sav = 0;		/* cant tot de lineas con cont con allocate single var */
	sq_q_alloc_f90_slc_mav = 0;		/* cant tot de lineas sin cont con allocate multi var */
	sq_q_alloc_f90_clc_mav = 0;		/* cant tot de lineas con cont con allocate multi var */

	sg_q_alloc_t01 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t02 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t03 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t04 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t05 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t06 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t07 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t08 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t09 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t10 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t11 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t12 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t13 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t14 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t15 = 0;			/* cant tot de allocs tipo ... */
	sg_q_alloc_t16 = 0;			/* cant tot de allocs tipo ... */

	/* para todas las lineas */
	
	tipo01 = 0;
	tipo03 = 0;
	tipo05 = 0;
	tipo07 = 0;

	tipo09 = 0;
	tipo11 = 0;
	tipo13 = 0;
	tipo15 = 0;

	for (j=0; j<20; j++)
		tipo[j]=0;

	i = 0;
	f_keep = 1;
	f_act  = 0;

	while (f_keep)
	{
		strcpy(b0,(*fnp[i]).l );

if (gp_debug && w)
{
	mprintf (z,"FOR prin (%3d) |%s| \n",i,b0);
	mprintf (z,"... \n\n");
}


		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));
		strcpy(exte_name,e_name(i));

		if (!strncmp(exte_name,"for",3))
			tipo_ext = 1;

		if (!strncmp(exte_name,"f90",3))
			tipo_ext = 2;

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}



		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
			num_alloc = 0;
			num_alloc_key = 1;
			strcpy(prog_name,(*tb[n_f]).n);

if (1)
{
printf ("Trabajo con ... (%s) |%s|\n",exte_name,prog_name);
}

		}


		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1, pasar_a_minusc(b0));
		strcpy(b2, pasar_a_minusc(b0));


		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;


		if (f_proceso)
		{
			/* temas especificos */	
			if (!strncmp(prog_name,"cap_objt.f90",12))
			{
				if (f_miscmod1 == 0 && !strncmp( (*fnp[i]).l,"      SUBROUTINE MIX_RATIOS",27))
				{
					f_miscmod1 = 1;
					strcpy( (*fnp[i+1]).l  ,"  ");
				}
			}

			if (!strncmp(prog_name,"cap_objt.f90",12))
			{
				if (f_miscmod2 == 0 && !strncmp( (*fnp[i]).l,"      FUNCTION PLANNING_DEC",27))
				{
					f_miscmod2 = 1;
					strcpy( (*fnp[i+1]).l, (*fnp[i+2]).l);
					strcpy( (*fnp[i+2]).l,"      use miscmod");
				}
			}

		}


if (gp_debug && w)
{
mprintf (z,"PPP: [%02d] |%-30.30s|  (%4d)  |%s| \n",f_proceso,prog_name,i,(*fnp[i]).l);
}

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	


			if (tiene_allocate(b1))
			{

if (gp_debug && w)
{
	mprintf (z,"allocate: |%s| \n",b1);
}

				num_alloc++;
				sq_q_alloc++;

				c2++;

				fprintf (hfout,"%05d %-30.30s (%s) %05d |%s|\n",
					c2,
					prog_name,
					exte_name,
					i-pf+1,
					b0);

				if (tipo_ext == 1)
					fprintf (hfaux,"%s\n",b0);

				if (tipo_ext == 2)
					fprintf (hflog,"%s\n",b0);

				/* es .for EFOR ------------------------------------  */
				if(tipo_ext == 1 ) 
				{
					c3++;

					if (!tiene_mas( (*fnp[i+1]).l ) )
					{
						/* no tiene lineas de continuacion ... */
						sq_q_alloc_for_slc++;
	
						fprintf (hfou2,"%05d %-30.30s (%s) %05d |%s|\n",
							c3, prog_name, exte_name, i-pf+1, (*fnp[i]).l);

						strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(b1))  
						{
							/* Es un allocate en una linea con una sola variable */
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_slc_sav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{
								/* TIPO-1 alloc - una linea - una sola variable - sin stat */
								sg_q_alloc_t01++;
								es_tipo=1;
								tipo[es_tipo]++;

								tipo01++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan(1,1,1,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 1 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,1,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}
if (gp_debug && w)
{
mprintf (z,"TIPO-1 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

							}
							else
							{
								/* TIPO-2 alloc - una linea - una sola variable - con stat */
								sg_q_alloc_t02++;
								es_tipo=2;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(b1));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));

									grabar_plan(2,1,1,1,1,prog_name,m1);

									grabar_mapa(1,2,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,2,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);
#if 1
									num_alloc_key = num_alloc_fnd+1;
#endif

								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(2,1,1,1,2,prog_name,m1);
								}


							}
						}
						else /* if (!tiene_multiple ... ) */
						{
							/* Es un allocate en una linea con una varias variables */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_slc_mav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{
								/* TIPO-3 es un alloc en una linea con mas de una var - sin stat */
								sg_q_alloc_t03++;
								es_tipo=3;
								tipo[es_tipo]++;

								tipo03++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan(3,1,2,0,0,prog_name,m1);

								f_act = 1;
#if 1

#if 0
								if (gp_proceed == 3 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,3,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}
if (gp_debug && w)
{
mprintf (z,"TIPO-3 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g03(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}
#endif
							}	
							else
							{
								/* TIPO-4 alloc - una linea - una sola variable - con stat */
								sg_q_alloc_t04++;
								es_tipo=4;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan(4,1,2,1,0,prog_name,m1);

							}
						}
					}
					else
					{
						/* tiene lineas de continuacion ... */
						sq_q_alloc_for_clc++;
						k=0;
						memset(m0,0,MSTR);

						do
						{
							fprintf (hfou2,"%05d %-30.30s (%s) %05d |%s|\n",
								c3, prog_name, exte_name, i-pf+1+k, (*fnp[i+k]).l);

							if (!es_linea_comentario( (*fnp[i+k]).l ))
							{
								strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
								strcpy(b1, trim_beg(b1));
								strcpy(b1, trim_end(b1));
								strcpy(b1, trim_blanks(b1));
								strcat(m0,b1);
							}
							k++;
						}
						while ( tiene_mas(  (*fnp[i+k]).l ) || 
							(es_linea_comentario( (*fnp[i+k]).l) && tiene_mas( (*fnp[i+k+1]).l))   );
					
if (gp_debug && w)
{
mprintf (z,"alloc: 1 m0: |%s|\n",m0);
}
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(m0))
						{
							/*  tiene lineas de cont ... solo una variable   */ 
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_clc_sav++;

							if (!f_stat)
							{
								/* TIPO-5 es un alloc en varias lineas con una sola variable - sin stat  */
								sg_q_alloc_t05++;
								es_tipo=5;
								tipo[es_tipo]++;

								tipo05++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(5,2,1,0,0,prog_name,m1);

if (gp_debug && w)
{
mprintf (z,"alloc: 2 m0: |%s|\n",m0);
mprintf (z,"alloc: 2 m1: |%s|\n",m1);
}
								f_act = 1;
#if 1

#if 0
								if (gp_proceed == 5 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,5,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

if (gp_debug && w)
{
mprintf (z,"TIPO-5 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g05(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}
#endif

							}
							else
							{
								/* TIPO-6 es un alloc en varias lineas con una sola variable - con stat  */
								sg_q_alloc_t06++;
								es_tipo=6;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(m0));

#if 0
								/* ojo, aca falta ver si el stat esta ok */
								grabar_plan(6,2,1,1,0,prog_name,m1);
#endif

#if 1

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(6,2,1,1,1,prog_name,m1);

									grabar_mapa(1,6,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(6,2,1,1,2,prog_name,m1);
								}


#endif

							}
						}
						else
						{
							/* es un alloc con lineas de cont y varias vars */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_clc_mav++;

							if (!f_stat)
							{
								/* TIPO-7 es un alloc en varias lineas con varias variables - sin stat */
								sg_q_alloc_t07++;
								es_tipo=7;
								tipo[es_tipo]++;

								tipo07++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(7,2,2,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 7 )
#endif
#if 1
								if (gp_proceed )
#endif

								{
									flag_alloc_ok = 0;
									grabar_mapa(1,7,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

if (gp_debug && w)
{
mprintf (z,"g7_1, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 


									/* agrego el check_alloc */
									chg_alloc_g07(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}

							}
							else
							{
								/* TIPO-8 es un alloc en varias lineas con varias variables - con stat */
								sg_q_alloc_t08++;
								es_tipo=8;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(8,2,2,0,0,prog_name,m1);
							}
						}
					}
				}


				/* es .f90 EF90 ------------------------------------  */
				if(tipo_ext == 2)
				{

					c4++;

					if (!tiene_amper( (*fnp[i]).l ) )
					{
						/* es un allocate sin linea de continuacion */
						sq_q_alloc_f90_slc++;
						fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
							c4, prog_name, exte_name, i-pf+1, (*fnp[i]).l);

						strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
						f_stat = tiene_stat(b1);


						if (!tiene_multiple_vars(b1))  
						{
							/* es un allocate en una linea con una sola variable */
							sq_q_alloc_f90_sav++;
							sq_q_alloc_f90_slc_sav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{

								/* TIPO-9 alloc en una sola linea una sola variable sin stat */
								sg_q_alloc_t09++;
								es_tipo=9;
								tipo[es_tipo]++;

								tipo09++;
								strcpy(m1,trim_blanks_beg(b1));
								strcpy(m1,quitar_blancos_molestos(m1));

								grabar_plan(9,1,1,0,0,prog_name,m1);

								f_act = 1;

#if 0
								if (gp_proceed == 9 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,9,prog_name,0,0,0);

									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

if (gp_debug && w)
{
mprintf (z,"TIPO-9 desp de chg_alloc ... add_lines: %d\n",add_lines);
}
									/* atenti:
									 * chg_alloc_t09 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente
									 * aumenta la cantidad de lineas del fuente ...
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
									chg_alloc_g09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

							}
							else   /* if (!f_stat) ... */
							{
								/* TIPO-10 alloc en una sola linea una sola variable con stat */
								sg_q_alloc_t10++;
								es_tipo=10;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;

								strcpy(m1,trim_blanks_beg(b1));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(10,1,1,1,1,prog_name,m1);

									grabar_mapa(1,10,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(10,1,1,1,2,prog_name,m1);
								}

							}
						}
						else /* if (tiene_continuacion ... */
						{
							/* es un allocate en una linea con varias vars - sin stat */
							sq_q_alloc_f90_mav++;
							sq_q_alloc_f90_slc_mav++;

							if (!f_stat)
							{
								/* TIPO-11 alloc en una linea con varias vars - sin stat */
								sg_q_alloc_t11++;
								es_tipo=11;
								tipo[es_tipo]++;

								tipo11++;
								strcpy(m1,trim_blanks_beg(b1));

								grabar_plan(11,1,2,0,0,prog_name,m1);

								f_act = 1;

#if 0
								if (gp_proceed == 11 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,11,prog_name,0,0,0);

									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);
if (gp_debug && w)
{
mprintf (z,"TIPO-11 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
									chg_alloc_g15(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

if (gp_debug && w)
{
mprintf (z,"g15_A, volvi de chg_alloc g15 add_lines: %d  \n",add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
if (gp_debug && w)
{
mprintf (z,"g15_2, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 

										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}
							}
							else
							{

								/* TIPO-12 alloc en una sola linea con varias vars - con stat */
								sg_q_alloc_t12++;
								es_tipo=12;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(b1));

								grabar_plan(12,1,2,1,0,prog_name,m1);
							}
						}
					}
					else
					{

						/* es un allocate con linea de continuacion */

						sq_q_alloc_f90_clc++;
						k=0;
						memset(m0,0,MSTR);

						while ( tiene_amper(  (*fnp[i+k]).l ) ||
							(es_linea_comentario ( (*fnp[i+k]).l ) && tiene_amper( (*fnp[i+k+1]).l ))  )
						{
							fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
								c4, prog_name, exte_name, i-pf+1+k, (*fnp[i+k]).l);

							if (!es_linea_comentario( (*fnp[i+k]).l ) )
							{
								strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
								strcpy(b1, trim_beg_f90(b1));
								strcpy(b1, trim_end_f90(b1));
								strcpy(b1, trim_blanks(b1));
								strcat(m0,b1);
								strcpy(m0,chanchada(m0));
							}
							k++;
						}

						strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
						strcpy(b1, trim_beg_f90(b1));
						strcpy(b1, trim_end_f90(b1));
						strcpy(b1, trim_blanks(b1));
						strcat(m0,b1);

					
						fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
							c4, prog_name, exte_name, i-pf+1+k, (*fnp[i+k]).l);

						/* verifico si tiene STAT= */
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(m0))
						{
							sq_q_alloc_f90_sav++;
							sq_q_alloc_f90_clc_sav++;


							if (!f_stat)
							{
								/* TIPO-13 es un alloc en varias lineas con una var - sin stat */
								sg_q_alloc_t13++;
								es_tipo=13;
								tipo[es_tipo]++;

								tipo13++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(13,2,1,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 13 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,13,prog_name,0,0,0);
									chg_alloc_t13(n_f,i,&add_lines,num_alloc,f_act);

									/* atenti:
									 * chg_alloc_t13 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente
									 * aumenta la cantidad de lineas del fuente ...
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
if (gp_debug && w)
{
mprintf (z,"g02_1, entro a  chg_alloc_g02 num_alloc %d add_lines: %d  \n",num_alloc,add_lines);
} 
									chg_alloc_g02(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);
if (gp_debug && w)
{
mprintf (z,"g02_2, volvi de chg_alloc_g02 num_alloc %d add_lines: %d  \n",num_alloc,add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

							}
							else /* if (!f_stat ... ) */
							{
								/* TIPO-14 es un alloc en varias lineas con una var - con stat */
								sg_q_alloc_t14++;
								es_tipo=14;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(m0));
#if 0
								grabar_plan(14,2,1,1,0,prog_name,m1);
#endif

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(14,2,1,1,1,prog_name,m1);

									grabar_mapa(1,14,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(14,2,1,1,2,prog_name,m1);
								}




							}
						}
						else /* if (!tiene_multiple_vars ... ) */
						{
							sq_q_alloc_f90_mav++;
							sq_q_alloc_f90_clc_mav++;

							
							if (!f_stat)
							{
								/* TIPO-15 es un alloc en varias lineas con varias vars - sin stat  */
								sg_q_alloc_t15++;
								es_tipo=15;
								tipo[es_tipo]++;

								tipo15++;
								strcpy(m1,trim_blanks_beg(m0));
								strcpy(m1,quitar_blancos_molestos(m1));

								grabar_plan(15,2,2,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 15 )
#endif
#if 1
								if (gp_proceed )
#endif

								{
									flag_alloc_ok = 0;
									grabar_mapa(1,15,prog_name,0,0,0);

									/* atenti:
									 * chg_alloc_t15 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente, modificar lineas del fuente
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									chg_alloc_t15(n_f,i,&add_lines,num_alloc,f_act);


									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
if (gp_debug && w)
{
mprintf (z,"g15_1, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 
										i+= add_lines;
										flag_alloc_ok = 0;
									}


									/* agrego el check_alloc */
									chg_alloc_g15(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

if (gp_debug && w)
{
mprintf (z,"g15_2, volvi de chg_alloc g15 add_lines: %d  \n",add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{

										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}

							}
							else
							{
								/* TIPO-16 es un alloc en varias lineas con varias vars - con stat  */
								sg_q_alloc_t16++;
								es_tipo=16;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(m0));
#if 0
								grabar_plan(16,2,2,1,0,prog_name,m1);
#endif


#if 1

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(16,2,2,1,1,prog_name,m1);

									grabar_mapa(1,14,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(16,2,2,1,2,prog_name,m1);
								}

#endif

							}


						}
					}
				}
			}

		} /* proceso */

		/* atenti !!! que pasa con los multi lineas !! 
		 * ver cambios de variables ... i ?
		 */
#if 0
		strcpy( (*fnp[i]).l , b0 );
		nf++;
#endif

#if 0
	if (gp_debug == 1)
		grabar_a_disco();
#endif

if (gp_debug && w)
{
mprintf (z,"g15_4,  qf_src-1 %d b: |%s|\n",qf_src,(*fnp[qf_src-1]).l);
mprintf (z,"..... \n\n");
} 
		i++;
		if (i >= qf_src)
			f_keep = 0;

if (gp_debug && w)
{
mprintf (z,"RRR i       %4d \n",i);
mprintf (z,"RRR qf_src  %4d \n",qf_src);
mprintf (z,"RRR f_keep  %4d \n",f_keep);
mprintf (z,"- - - - \n");
}


	} /* for .. while ... principal */




if (0)
{
	printf ("\n");
	printf ("tipo 01 %3d \n",tipo01);
	printf ("tipo 03 %3d \n",tipo03);
	printf ("tipo 05 %3d \n",tipo05);
	printf ("tipo 07 %3d \n",tipo07);

	printf ("tipo 09 %3d \n",tipo09);
	printf ("tipo 11 %3d \n",tipo11);
	printf ("tipo 13 %3d \n",tipo13);
	printf ("tipo 15 %3d \n",tipo15);
	printf ("\n");


	for (j=1; j<=16; j++)
	{
		printf ("tipo %2d:  %4d \n",j,tipo[j]);
	}
	printf ("\n");
}

/*
 * desdoblar lineas largas funciona, tanto para .f90 como para .for
 * en esta seccion (poner siguiente if a 1 )
 * si se esta procesando un solo programa a la vez
 * atencion:
 * usar en combinacion con ex9_p1_c !!!!
 */

/* desdoblar lineas largas */
#if 0
	/*
	 * Limites de largo de lineas sugeridos !!
	 * en f77 ... limite del compilador, max 72 caracteres
	 * en f90 ... limite solicitado para code review .. 83 aprox
	 *
	 */


	f_proceso = 1;
	if (!gp_debug)
		f_proceso=0;

	if (f_proceso)
	{

	/* se supone que a esta altura ... 
	 * tengo todo el src ya modificado en fnp ... 
	 *
	 * se puede trabajar aqui para ver si queremos separar las lineas 
	 * muy largas 
	 */

	/* probamos a ver como funca hacer lugar ... */

	if (tipo_ext == 2)
	{

	i = 0;
	f_keep = 1;
	f_act  = 1;

	while (f_keep)
	{

	pf = (*tb[n_f]).pf;
	uf = (*tb[n_f]).uf;

		strcpy(b0, pasar_a_minusc( (*fnp[i]).l) );
		strcpy(b1,  (*fnp[i]).l );

		strcpy(b0,trim_end_f90(b0));
		strcpy(b0,trim_blanks(b0));

		if (tiene_string(b0,"check_alloc"))
		{
if (gp_debug && w)
{
mprintf (z,"(1)entre a check_alloc_ok con  ... |%s| \n", (*fnp[i]).l );
mprintf (z,"(2)entre a check_alloc_ok con  ... |%s| \n", b0 );
}
			l2 = strlen(b0);

			if (l2 > 82 )
			{
if (gp_debug && w)
{
mprintf (z,"voy a hacer lugar: pf %d uf %d i %d \n",pf,uf,i);
mprintf (z,"linea: len: %3d |%s|\n", l2, (*fnp[i]).l );
}
				hacer_lugar_itz(pf,uf,i,1,f_act);

				memset(b5,0,sizeof(b5));
				sprintf (b5,"agregado ... check_alloc :%4d",n2);
	
				memset(b2,0,sizeof(b2));
				memset(b3,0,sizeof(b3));

				arma_dos_lineas_con_check(b1,b2,b3);

if (gp_debug && w)
{
mprintf (z,"voy a strcpy 1 i: %d str: |%s| \n",i,(*fnp[i]).l );
}
				strcpy( (*fnp[i+0]).l, b2);
if (gp_debug && w)
{
mprintf (z,"hice  strcpy 1 i: %d str: |%s| \n",i,(*fnp[i]).l );
}

				strcpy( (*fnp[i+1]).l, b3);

				i+= 1;
			}
		}


		i++;
		if (i >= qf_src)
			f_keep = 0;
	}

	} /* if (tipo_ext == 2.... */



	} /* f_proceso ... */





	f_proceso=1;
	if (!gp_debug)
		f_proceso=0;

	if (f_proceso)
	{

	if (tipo_ext == 1)
	{

		printf ("Listo lineas con mas de 72 chars \n");


		/* reviso los check alloc */
		i      = 0;
		f_keep = 1;

		do
		{
			pf =  (*tb[n_f]).pf ;
			uf =  (*tb[n_f]).uf ;

			strcpy(b0,(*fnp[i]).l);
			strcpy(b1,pasar_a_minusc ( (*fnp[i]).l) );
	
			f_proceso = 1;
			if (linea_vacia(b0) || es_linea_comentario(b0))
				f_proceso = 0;

			if (f_proceso)
			{
				strcpy(b0,trim_end_f90(b0));
				l2 = strlen(b0);

				if (l2 > 72)
				{
					if (tiene_string(b0,"check_alloc"))
					{

						hacer_lugar_itz(pf,uf,i,1,1);

						memset(b2,0,sizeof(b2));
						memset(b3,0,sizeof(b3));
						arma_dos_lineas_con_check_for(b0,b2,b3);
if (gp_debug && w)
{
mprintf (z,"linea orig:    |%s| \n",b0);
mprintf (z,"linea1         |%s| \n",b2);
mprintf (z,"linea2         |%s| \n",b3);
}
						strcpy( (*fnp[i+0]).l, b2);
						strcpy( (*fnp[i+1]).l, b3);
						i+= 1;

						pf =  (*tb[n_f]).pf ;
						uf =  (*tb[n_f]).uf ;
					}

				}
			}

			i++;
			if (i >= uf)
				f_keep = 0;
		}
		while (f_keep);



		/* reviso los allocate */
		i      = 0;
		f_keep = 1;

		do
		{
			pf =  (*tb[n_f]).pf ;
			uf =  (*tb[n_f]).uf ;

			strcpy(b0,(*fnp[i]).l);
			strcpy(b1,pasar_a_minusc ( (*fnp[i]).l) );
	
			f_proceso = 1;
			if (linea_vacia(b0) || es_linea_comentario(b0))
				f_proceso = 0;

			if (f_proceso)
			{
				strcpy(b0,trim_end_f90(b0));
				l2 = strlen(b0);

				if (l2 > 72)
				{
					if (tiene_string(b1,"allocate") && tiene_string(b1,"stv_er") )
					{
						/* puedo armar dos lineas, poniendo stat=stv_er aparte */
						if (l2 > 72 && l2 < 86)
						{
							printf ("EEF1:%03d|%s|\n",l2,b0);

							hacer_lugar_itz(pf,uf,i,1,1);
							memset(b2,0,sizeof(b2));
							memset(b3,0,sizeof(b3));
							arma_dos_lineas_con_alloc_for_v1(b0,b2,b3);

							strcpy( (*fnp[i+0]).l, b2);
							strcpy( (*fnp[i+1]).l, b3);

							i+= 1;
							pf =  (*tb[n_f]).pf ;
							uf =  (*tb[n_f]).uf ;
						}

						/* necesito dos lineas, pero cortar en algun lugar en el medio */
						if (l2 >= 86)
						{
							printf ("EEF2:%03d|%s|\n",l2,b0);

#if 1
							hacer_lugar_itz(pf,uf,i,1,1);

							memset(b2,0,sizeof(b2));
							memset(b3,0,sizeof(b3));
							arma_dos_lineas_con_alloc_for_v2(b0,b2,b3);

							strcpy( (*fnp[i+0]).l, b2);
							strcpy( (*fnp[i+1]).l, b3);

							i+= 1;
							pf =  (*tb[n_f]).pf ;
							uf =  (*tb[n_f]).uf ;
#endif
						}

					}
				}
			}

			i++;
			if (i >= uf)
				f_keep = 0;
		}
		while (f_keep);




		/* reviso los los stat=stv_er que no estan en la linea del allocate  */

		pf =  (*tb[n_f]).pf ;
		uf =  (*tb[n_f]).uf ;

		i      = pf;
		f_keep = 1;

		do
		{

			strcpy(b0,(*fnp[i]).l);
			strcpy(b1,pasar_a_minusc ( (*fnp[i]).l) );
			strcpy(b1,trim_end_f90(b1));
	
			f_proceso = 1;
			if (linea_vacia(b1) || es_linea_comentario(b1))
				f_proceso = 0;

			if (f_proceso)
			{
				l2 = strlen(b1);

				if (l2 > 72)
				{
					if (tiene_string(b1,"stat=stv_er") && !tiene_string(b1,"allocate") )
					{

						/* pos del primer caracter de sentencia ... */
						for (j=7, p1=0, l2=strlen(b0), f1=1; f1 && j<l2; j++)
							if (b0[j] != ' ')
								f1 = 0, p1 = j;

						/* pos del stat=stv_er */
						for (j=0, p2=0, f1=1; f1 && j<l2; j++)
							if (!strncmp(b0+j,",stat=stv_er",12))
								f1 = 0, p2 = j;

						/* si hay comentario ... pos del comentario ... */
						for (j=p2+12, p3=0, f1=1; f1 && j<l2; j++)
							if (b0[j] == '!')
								f1 = 0, p3 = j;


						/* copio la sentencia pura, sin el stat= ... */
						memset(b2,0,sizeof(b2));
						strncpy(b2,b0+p1,p2-p1+1);
printf ("CCC 0 : %2d  |%s|\n",0,b2);

						/* copio si tiene comentarios  ... */
						memset(b3,0,sizeof(b3));
						if (p3)
							strcpy(b3,b0+p3);
						else
							strcpy(b3," ");

printf ("CCC 1 : %2d |%s|\n",1,b3);

						
						/* dos casos 
						 * 1) puedo correr toda la linea ... 
						 * 2) no entra !! tengo que armar una tercera linea !!
						 */

						/* caso 1 ... */
						if (p2-p1+1 <= 46)
						{
							memset(b4,32,sizeof(b4));
							memset(b5,0 ,sizeof(b5));
							strcpy(b5,"     +");
							strncat(b5,b4,50-(p2-p1+1));
							strcat(b5,b2);
							strcat(b5,"stat=stv_er) ");
							if (p3)
								strcat(b5,b3);
						
							strcpy( (*fnp[i+0]).l, b5);
printf ("DDD 2 : %2d |%s|\n",2,b5);

						}
						else
						{
							memset(b4,32,sizeof(b4));
							memset(b5,0,sizeof(b5));
							memset(b6,0,sizeof(b6));
							strcpy(b5,"     +");
							strncat(b5,b4,62-(p2-p1+1));
							strcat(b5,b2);

							strcat(b6,"     +");
							strncat(b6,b4,50);
							strcat(b6,"stat=stv_er) ");
							if (p3)
								strcat(b6,b3);


							pf =  (*tb[n_f]).pf ;
							uf =  (*tb[n_f]).uf ;

							hacer_lugar_itz(pf,uf,i,1,1);

							strcpy( (*fnp[i+0]).l,b5);
							strcpy( (*fnp[i+1]).l,b6);
						
							i+= 1;
printf ("DDD 3 : %2d |%s|\n",3,b0+p1);
printf ("DDD 5 : %2d |%s|\n",5,b5);
printf ("DDD 6 : %2d |%s|\n",6,b6);

						}
						

					}
				}
			}

			i++;
			if (i >= uf)
				f_keep = 0;
		}
		while (f_keep);



		printf ("\n\n");
	}

	} /* f_proceso ... */



if (gp_debug && w)
{
	printf ("Listo las lineas del fuente \n");
	for (i = (*tb[n_f]).pf ; i<= (*tb[n_f]).uf ; i++)
	{
		strcpy(b0,(*fnp[i]).l);

		strcpy(b0,trim_end_f90(b0));

		l2 = strlen(b0);

		printf ("%3d |%s|\n",l2,b0);
	}

	printf ("\n\n");
	
}



if (gp_debug && w)
{
	printf ("Listo las lineas largas del fuente \n");
	for (i = (*tb[n_f]).pf ; i<= (*tb[n_f]).uf ; i++)
	{
		strcpy(b0,(*fnp[i]).l);

		f_proceso = 1;
		if (linea_vacia(b0) || es_linea_comentario(b0))
			f_proceso = 0;

		if (f_proceso)
		{
			strcpy(b0,trim_end_f90(b0));
			l2 = strlen(b0);

			if (l2 > 72)
				printf ("%3d|%s|\n",l2,b0);
		}
	}

	printf ("\n\n");
	
}

#endif
/* desdoblar lineas largas */


	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex9_p1_c
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec9
 * hace algo con todas las lineas cargadas en memoria
 *
 * soluciona tema lieneas largas
 *
 */



int	ex9_p1_c()
{
	static	int	f_miscmod1 = 0;
	static	int	f_miscmod2 = 0;
	static	int	f_msgmmsb6 = 0;

	int 	h,i,j,k,k1,k2;
	int	l1,l2,l3;
	int	c1,c2,c3,c4;
	int	f1,f2,f3,f4,f5;
	int	p1,p2,p3,p4;
	int	f_proceso;
	int	f_stat;
	int	f_keep;
	int	f_act;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	b7[MAXB];
	int	pf,uf,nf;
	int	tipo_ext;
	int	add_lines;		/* cant de lineas que hay que sumar al fuente al corregir un allocate */
	int	num_alloc;		/* numero de alloc encontrado en src */
	int	num_alloc_key;		/* numero de key usado en file para identificar alloc en fuente */
	int	num_alloc_fnd;		/* num de alloc cuando es encontrado en src */
	int	nf_check;		/* linea donde esta el check alloc encontrado en el fuente */
	int	r1;
	int	k_amp;
	int	n1,n2;
	
	int	tipo_split;

	int	es_tipo,tipo[20];

	int	tipo01;
	int	tipo02;
	int	tipo03;
	int	tipo04;
	int	tipo05;
	int	tipo06;
	int	tipo07;
	int	tipo08;

	int	tipo09;
	int	tipo11;
	int	tipo13;
	int	tipo15;

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"ex9_p1_c");
	w = g_dbg(z);


	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* stats */
	sq_q_alloc         = 0;
	sq_q_alloc_for_sta = 0;
	sq_q_alloc_f90_sta = 0;

	sq_q_alloc_for_slc = 0;
	sq_q_alloc_for_clc = 0;
	sq_q_alloc_for_sav = 0;
	sq_q_alloc_for_mav = 0;
	sq_q_alloc_for_slc_sav = 0;		/* cant tot de lineas sin cont con allocate single var */
	sq_q_alloc_for_clc_sav = 0;		/* cant tot de lineas con cont con allocate single var */
	sq_q_alloc_for_slc_mav = 0;		/* cant tot de lineas sin cont con allocate multi var */
	sq_q_alloc_for_clc_mav = 0;		/* cant tot de lineas con cont con allocate multi var */

	sq_q_alloc_f90_slc = 0;
	sq_q_alloc_f90_clc = 0;
	sq_q_alloc_f90_sav = 0;			/* cantidad total de lineas allocate con single var */
	sq_q_alloc_f90_mav = 0;			/* cantidad total de lineas allocate con multiple var */
	sq_q_alloc_f90_slc_sav = 0;		/* cant tot de lineas sin cont con allocate single var */
	sq_q_alloc_f90_clc_sav = 0;		/* cant tot de lineas con cont con allocate single var */
	sq_q_alloc_f90_slc_mav = 0;		/* cant tot de lineas sin cont con allocate multi var */
	sq_q_alloc_f90_clc_mav = 0;		/* cant tot de lineas con cont con allocate multi var */


	/* para todas las lineas */
	
	tipo01 = 0;
	tipo03 = 0;
	tipo05 = 0;
	tipo07 = 0;

	tipo09 = 0;
	tipo11 = 0;
	tipo13 = 0;
	tipo15 = 0;

	for (j=0; j<20; j++)
		tipo[j]=0;


#if 0
/* bloque while principal */
	i = 0;
	f_keep = 1;
	f_act  = 0;

	while (f_keep)
	{
		strcpy(b0,(*fnp[i]).l );

if (gp_debug && w)
{
	mprintf (z,"FOR prin (%3d) |%s| \n",i,b0);
	mprintf (z,"... \n\n");
}


		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));
		strcpy(exte_name,e_name(i));

		if (!strncmp(exte_name,"for",3))
			tipo_ext = 1;

		if (!strncmp(exte_name,"f90",3))
			tipo_ext = 2;

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}



		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
			num_alloc = 0;
			num_alloc_key = 1;
			strcpy(prog_name,(*tb[n_f]).n);
printf ("AAA prog_name |%s| \n",prog_name);
printf ("AAA base_name |%s| \n",base_name);


if (1)
{
printf ("Trabajo con ... (%s) |%s|\n",exte_name,prog_name);
}

		}


		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1, pasar_a_minusc(b0));
		strcpy(b2, pasar_a_minusc(b0));


		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;


		if (f_proceso)
		{
			/* temas especificos */	
			if (!strncmp(prog_name,"cap_objt.f90",12))
			{
				if (f_miscmod1 == 0 && !strncmp( (*fnp[i]).l,"      SUBROUTINE MIX_RATIOS",27))
				{
					f_miscmod1 = 1;
					strcpy( (*fnp[i+1]).l  ,"  ");
				}
			}

			if (!strncmp(prog_name,"cap_objt.f90",12))
			{
				if (f_miscmod2 == 0 && !strncmp( (*fnp[i]).l,"      FUNCTION PLANNING_DEC",27))
				{
					f_miscmod2 = 1;
					strcpy( (*fnp[i+1]).l, (*fnp[i+2]).l);
					strcpy( (*fnp[i+2]).l,"      use miscmod");
				}
			}

		}


		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			if (tiene_allocate(b1))
			{

if (gp_debug && w)
{
	mprintf (z,"allocate: |%s| \n",b1);
}

				num_alloc++;
				sq_q_alloc++;

				c2++;

				fprintf (hfout,"%05d %-30.30s (%s) %05d |%s|\n",
					c2,
					prog_name,
					exte_name,
					i-pf+1,
					b0);

				if (tipo_ext == 1)
					fprintf (hfaux,"%s\n",b0);

				if (tipo_ext == 2)
					fprintf (hflog,"%s\n",b0);

				/* es .for EFOR ------------------------------------  */
				if(tipo_ext == 1 ) 
				{
					c3++;

					if (!tiene_mas( (*fnp[i+1]).l ) )
					{
						/* no tiene lineas de continuacion ... */
						sq_q_alloc_for_slc++;
	
						fprintf (hfou2,"%05d %-30.30s (%s) %05d |%s|\n",
							c3, prog_name, exte_name, i-pf+1, (*fnp[i]).l);

						strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(b1))  
						{
							/* Es un allocate en una linea con una sola variable */
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_slc_sav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{
								/* TIPO-1 alloc - una linea - una sola variable - sin stat */
								es_tipo=1;
								tipo[es_tipo]++;

								tipo01++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan(1,1,1,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 1 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,1,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}
if (gp_debug && w)
{
mprintf (z,"TIPO-1 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

							}
							else
							{
								/* TIPO-2 alloc - una linea - una sola variable - con stat */
								es_tipo=2;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(b1));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));

									grabar_plan(2,1,1,1,1,prog_name,m1);

									grabar_mapa(1,2,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,2,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);
#if 1
									num_alloc_key = num_alloc_fnd+1;
#endif

								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(2,1,1,1,2,prog_name,m1);
								}


							}
						}
						else /* if (!tiene_multiple ... ) */
						{
							/* Es un allocate en una linea con una varias variables */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_slc_mav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{
								/* TIPO-3 es un alloc en una linea con mas de una var - sin stat */
								es_tipo=3;
								tipo[es_tipo]++;

								tipo03++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan(3,1,2,0,0,prog_name,m1);

								f_act = 1;
#if 1

#if 0
								if (gp_proceed == 3 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,3,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}
if (gp_debug && w)
{
mprintf (z,"TIPO-3 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g03(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}
#endif
							}	
							else
							{
								/* TIPO-4 alloc - una linea - una sola variable - con stat */
								es_tipo=4;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan(4,1,2,1,0,prog_name,m1);

							}
						}
					}
					else
					{
						/* tiene lineas de continuacion ... */
						sq_q_alloc_for_clc++;
						k=0;
						memset(m0,0,MSTR);

						do
						{
							fprintf (hfou2,"%05d %-30.30s (%s) %05d |%s|\n",
								c3, prog_name, exte_name, i-pf+1+k, (*fnp[i+k]).l);

							if (!es_linea_comentario( (*fnp[i+k]).l ))
							{
								strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
								strcpy(b1, trim_beg(b1));
								strcpy(b1, trim_end(b1));
								strcpy(b1, trim_blanks(b1));
								strcat(m0,b1);
							}
							k++;
						}
						while ( tiene_mas(  (*fnp[i+k]).l ) || 
							(es_linea_comentario( (*fnp[i+k]).l) && tiene_mas( (*fnp[i+k+1]).l))   );
					
if (gp_debug && w)
{
mprintf (z,"alloc: 1 m0: |%s|\n",m0);
}
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(m0))
						{
							/*  tiene lineas de cont ... solo una variable   */ 
							sq_q_alloc_for_sav++;
							sq_q_alloc_for_clc_sav++;

							if (!f_stat)
							{
								/* TIPO-5 es un alloc en varias lineas con una sola variable - sin stat  */
								es_tipo=5;
								tipo[es_tipo]++;

								tipo05++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(5,2,1,0,0,prog_name,m1);

if (gp_debug && w)
{
mprintf (z,"alloc: 2 m0: |%s|\n",m0);
mprintf (z,"alloc: 2 m1: |%s|\n",m1);
}
								f_act = 1;
#if 1

#if 0
								if (gp_proceed == 5 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,5,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

if (gp_debug && w)
{
mprintf (z,"TIPO-5 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* agrego el check_alloc */
									chg_alloc_g05(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}
#endif

							}
							else
							{
								/* TIPO-6 es un alloc en varias lineas con una sola variable - con stat  */
								es_tipo=6;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(m0));

#if 0
								/* ojo, aca falta ver si el stat esta ok */
								grabar_plan(6,2,1,1,0,prog_name,m1);
#endif

#if 1

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(6,2,1,1,1,prog_name,m1);

									grabar_mapa(1,6,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(6,2,1,1,2,prog_name,m1);
								}


#endif

							}
						}
						else
						{
							/* es un alloc con lineas de cont y varias vars */
							sq_q_alloc_for_mav++;
							sq_q_alloc_for_clc_mav++;

							if (!f_stat)
							{
								/* TIPO-7 es un alloc en varias lineas con varias variables - sin stat */
								es_tipo=7;
								tipo[es_tipo]++;

								tipo07++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(7,2,2,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 7 )
#endif
#if 1
								if (gp_proceed )
#endif

								{
									flag_alloc_ok = 0;
									grabar_mapa(1,7,prog_name,0,0,0);

									/*
									 * chg_alloc_t09 procesa 1 allocate por vez
									 * n_f		es el numero de file en tb
									 * i		es la fila que estamos procesando
									 * add_lines	son las lineas que hay que agregar a indice 'i'
									 * num_alloc	es el numero de alloc consecutivo en src
									 * num_alloc_key  es la base para actualizar en check_alloc
									 */
									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

if (gp_debug && w)
{
mprintf (z,"g7_1, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 


									/* agrego el check_alloc */
									chg_alloc_g07(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}

							}
							else
							{
								/* TIPO-8 es un alloc en varias lineas con varias variables - con stat */
								es_tipo=8;
								tipo[es_tipo]++;

								sq_q_alloc_for_sta++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(8,2,2,0,0,prog_name,m1);
							}
						}
					}
				}


				/* es .f90 EF90 ------------------------------------  */
				if(tipo_ext == 2)
				{

					c4++;

					if (!tiene_amper( (*fnp[i]).l ) )
					{
						/* es un allocate sin linea de continuacion */
						sq_q_alloc_f90_slc++;
						fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
							c4, prog_name, exte_name, i-pf+1, (*fnp[i]).l);

						strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
						f_stat = tiene_stat(b1);


						if (!tiene_multiple_vars(b1))  
						{
							/* es un allocate en una linea con una sola variable */
							sq_q_alloc_f90_sav++;
							sq_q_alloc_f90_slc_sav++;

							/* verifico si ya tiene stat */
							if (!f_stat)
							{

								/* TIPO-9 alloc en una sola linea una sola variable sin stat */
								es_tipo=9;
								tipo[es_tipo]++;

								tipo09++;
								strcpy(m1,trim_blanks_beg(b1));
								grabar_plan(9,1,1,0,0,prog_name,m1);

								f_act = 1;

#if 0
								if (gp_proceed == 9 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,9,prog_name,0,0,0);

									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

if (gp_debug && w)
{
mprintf (z,"TIPO-9 desp de chg_alloc ... add_lines: %d\n",add_lines);
}
									/* atenti:
									 * chg_alloc_t09 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente
									 * aumenta la cantidad de lineas del fuente ...
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
									chg_alloc_g09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

							}
							else   /* if (!f_stat) ... */
							{
								/* TIPO-10 alloc en una sola linea una sola variable con stat */
								es_tipo=10;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;

								strcpy(m1,trim_blanks_beg(b1));

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(10,1,1,1,1,prog_name,m1);

									grabar_mapa(1,10,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(10,1,1,1,2,prog_name,m1);
								}

							}
						}
						else /* if (tiene_continuacion ... */
						{
							/* es un allocate en una linea con varias vars - sin stat */
							sq_q_alloc_f90_mav++;
							sq_q_alloc_f90_slc_mav++;

							if (!f_stat)
							{
								/* TIPO-11 alloc en una linea con varias vars - sin stat */
								es_tipo=11;
								tipo[es_tipo]++;

								tipo11++;
								strcpy(m1,trim_blanks_beg(b1));

								grabar_plan(11,1,2,0,0,prog_name,m1);

								f_act = 1;

#if 0
								if (gp_proceed == 11 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;

									grabar_mapa(1,11,prog_name,0,0,0);

									chg_alloc_t09(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);
if (gp_debug && w)
{
mprintf (z,"TIPO-11 desp de chg_alloc ... add_lines: %d\n",add_lines);
}

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
									chg_alloc_g15(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

if (gp_debug && w)
{
mprintf (z,"g15_A, volvi de chg_alloc g15 add_lines: %d  \n",add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
if (gp_debug && w)
{
mprintf (z,"g15_2, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 

										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}
							}
							else
							{

								/* TIPO-12 alloc en una sola linea con varias vars - con stat */
								es_tipo=12;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(b1));

								grabar_plan(12,1,2,1,0,prog_name,m1);
							}
						}
					}
					else
					{

						/* es un allocate con linea de continuacion */

						sq_q_alloc_f90_clc++;
						k=0;
						memset(m0,0,MSTR);

						while ( tiene_amper(  (*fnp[i+k]).l ) ||
							(es_linea_comentario ( (*fnp[i+k]).l ) && tiene_amper( (*fnp[i+k+1]).l ))  )
						{
							fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
								c4, prog_name, exte_name, i-pf+1+k, (*fnp[i+k]).l);

							if (!es_linea_comentario( (*fnp[i+k]).l ) )
							{
								strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
								strcpy(b1, trim_beg_f90(b1));
								strcpy(b1, trim_end_f90(b1));
								strcpy(b1, trim_blanks(b1));
								strcat(m0,b1);
								strcpy(m0,chanchada(m0));
							}
							k++;
						}

						strcpy(b1, pasar_a_minusc( (*fnp[i+k]).l) );
						strcpy(b1, trim_beg_f90(b1));
						strcpy(b1, trim_end_f90(b1));
						strcpy(b1, trim_blanks(b1));
						strcat(m0,b1);

					
						fprintf (hfou3,"%05d %-30.30s (%s) %05d |%s|\n",
							c4, prog_name, exte_name, i-pf+1+k, (*fnp[i+k]).l);

						/* verifico si tiene STAT= */
						f_stat = tiene_stat(b1);

						if (!tiene_multiple_vars(m0))
						{
							sq_q_alloc_f90_sav++;
							sq_q_alloc_f90_clc_sav++;


							if (!f_stat)
							{
								/* TIPO-13 es un alloc en varias lineas con una var - sin stat */
								es_tipo=13;
								tipo[es_tipo]++;

								tipo13++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(13,2,1,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 13 )
#endif
#if 1
								if (gp_proceed)
#endif
								{
									flag_alloc_ok = 0;
									grabar_mapa(1,13,prog_name,0,0,0);
									chg_alloc_t13(n_f,i,&add_lines,num_alloc,f_act);

									/* atenti:
									 * chg_alloc_t13 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente
									 * aumenta la cantidad de lineas del fuente ...
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									/* agrego el check_alloc */
if (gp_debug && w)
{
mprintf (z,"g02_1, entro a  chg_alloc_g02 num_alloc %d add_lines: %d  \n",num_alloc,add_lines);
} 
									chg_alloc_g02(n_f,i,&add_lines,num_alloc,num_alloc_key,f_act);
if (gp_debug && w)
{
mprintf (z,"g02_2, volvi de chg_alloc_g02 num_alloc %d add_lines: %d  \n",num_alloc,add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
										i+= add_lines;
										flag_alloc_ok = 0;
									}

									num_alloc_key++;
								}

							}
							else /* if (!f_stat ... ) */
							{
								/* TIPO-14 es un alloc en varias lineas con una var - con stat */
								es_tipo=14;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(m0));
#if 0
								grabar_plan(14,2,1,1,0,prog_name,m1);
#endif

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(14,2,1,1,1,prog_name,m1);

									grabar_mapa(1,14,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(14,2,1,1,2,prog_name,m1);
								}




							}
						}
						else /* if (!tiene_multiple_vars ... ) */
						{
							sq_q_alloc_f90_mav++;
							sq_q_alloc_f90_clc_mav++;

							
							if (!f_stat)
							{
								/* TIPO-15 es un alloc en varias lineas con varias vars - sin stat  */
								es_tipo=15;
								tipo[es_tipo]++;

								tipo15++;
								strcpy(m1,trim_blanks_beg(m0));
								grabar_plan(15,2,2,0,0,prog_name,m1);

								f_act = 1;
#if 0
								if (gp_proceed == 15 )
#endif
#if 1
								if (gp_proceed )
#endif

								{
									flag_alloc_ok = 0;
									grabar_mapa(1,15,prog_name,0,0,0);

									/* atenti:
									 * chg_alloc_t15 debe procesar 1 solo allocate por vez...
									 * si lo proceso efectivamente, modificar lineas del fuente
									 * n_f es el numbero de file dentro de tb
									 * i   es la fila que estamos procesando
									 */

									chg_alloc_t15(n_f,i,&add_lines,num_alloc,f_act);


									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{
if (gp_debug && w)
{
mprintf (z,"g15_1, agrego a i: %d  add_lines: %d  qf_src %d b: |%s|\n",i,add_lines,qf_src,(*fnp[i+add_lines]).l);
} 
										i+= add_lines;
										flag_alloc_ok = 0;
									}


									/* agrego el check_alloc */
									chg_alloc_g15(n_f,i,&add_lines,num_alloc,num_alloc_key,&num_alloc_fnd,&k_amp,f_act);

if (gp_debug && w)
{
mprintf (z,"g15_2, volvi de chg_alloc g15 add_lines: %d  \n",add_lines);
} 

									/* actualizo variables de contexto */
									if (flag_alloc_ok)
									{

										i+= ( add_lines + k_amp);
										flag_alloc_ok = 0;
									}

									num_alloc_key = num_alloc_fnd+1;
								}

							}
							else
							{
								/* TIPO-16 es un alloc en varias lineas con varias vars - con stat  */
								es_tipo=16;
								tipo[es_tipo]++;

								sq_q_alloc_f90_sta++;
								strcpy(m1,trim_blanks_beg(m0));
#if 0
								grabar_plan(16,2,2,1,0,prog_name,m1);
#endif


#if 1

								/* si ya tiene_check_alloc bien armado, 
								 * solo anoto en plan, y recupero ultimo numero de key utilizado
								 * si no ...
								 * investigar si son casos que hay que re escribir el call check_alloc
								 */
								if (tiene_check_alloc_ok (i,&nf_check,&num_alloc_fnd))
								{
									strcpy(b4,pasar_a_minusc( (*fnp[nf_check]).l));
									strcpy(b4, trim_beg_f90(b4));
									strcpy(b4, trim_end_f90(b4));
									strcpy(b4, trim_blanks(b4));


									grabar_plan(16,2,2,1,1,prog_name,m1);

									grabar_mapa(1,14,prog_name,0,0,0);     
									grabar_mapa(6,0," ",num_alloc,0,0);
									grabar_mapa(8,10,b4,i,num_alloc_fnd,0 ); 
									grabar_mapa(0,0,"-",0,0,2);

									num_alloc_key = num_alloc_fnd+1;
								}
								else
								{	
									/* tiene stat= ... pero no tiene check alloc, hay que revisar */
									grabar_plan(16,2,2,1,2,prog_name,m1);
								}

#endif

							}


						}
					}
				}
			}

		} /* proceso */

		/* atenti !!! que pasa con los multi lineas !! 
		 * ver cambios de variables ... i ?
		 */
#if 0
		strcpy( (*fnp[i]).l , b0 );
		nf++;
#endif

#if 0
	if (gp_debug == 1)
		grabar_a_disco();
#endif

if (gp_debug && w)
{
mprintf (z,"g15_4,  qf_src-1 %d b: |%s|\n",qf_src,(*fnp[qf_src-1]).l);
mprintf (z,"..... \n\n");
} 
		i++;
		if (i >= qf_src)
			f_keep = 0;

	} /* for .. while ... principal */




if (1)
{
	printf ("\n");
	printf ("tipo 01 %3d \n",tipo01);
	printf ("tipo 03 %3d \n",tipo03);
	printf ("tipo 05 %3d \n",tipo05);
	printf ("tipo 07 %3d \n",tipo07);

	printf ("tipo 09 %3d \n",tipo09);
	printf ("tipo 11 %3d \n",tipo11);
	printf ("tipo 13 %3d \n",tipo13);
	printf ("tipo 15 %3d \n",tipo15);
	printf ("\n");


	for (j=1; j<=16; j++)
	{
		printf ("tipo %2d:  %4d \n",j,tipo[j]);
	}
	printf ("\n");
}



#endif
/* bloque while principal */




/* desdoblar lineas largas ex9_p1_c */
#if 1
	/*
	 * Limites de largo de lineas sugeridos !!
	 * en f77 ... limite del compilador, max 72 caracteres
	 * en f90 ... limite solicitado para code review .. 83 aprox
	 *
	 */

	int	p_f;
	int	u_f;

	n_f = 0;

	p_f = 0;
	u_f = qf_ff - 1;

	pf = 0;
	uf = qf_src - 1;

	/* check de que vector de lineas esta consistente */
	if (pf != (*tb[p_f]).pf || uf != (*tb[u_f]).uf )
		error(30001);



/* file tipo 1 */
#if 0


	if (tipo_ext == 1)
	{

		printf ("Listo lineas con mas de 72 chars \n");


		/* reviso los check alloc */
		i      = 0;
		f_keep = 1;

		do
		{
			pf =  (*tb[p_f]).pf ;
			uf =  (*tb[u_f]).uf ;

			strcpy(b0,(*fnp[i]).l);
			strcpy(b1,pasar_a_minusc ( (*fnp[i]).l) );
	
			f_proceso = 1;
			if (linea_vacia(b0) || es_linea_comentario(b0))
				f_proceso = 0;

			if (f_proceso)
			{
				strcpy(b0,trim_end_f90(b0));
				l2 = strlen(b0);

				if (l2 > 72)
				{
					if (tiene_string(b0,"check_alloc"))
					{

						hacer_lugar_itz(pf,uf,i,1,1);

						memset(b2,0,sizeof(b2));
						memset(b3,0,sizeof(b3));
						arma_dos_lineas_con_check_for(b0,b2,b3);
if (gp_debug && w)
{
mprintf (z,"linea orig:    |%s| \n",b0);
mprintf (z,"linea1         |%s| \n",b2);
mprintf (z,"linea2         |%s| \n",b3);
}
						strcpy( (*fnp[i+0]).l, b2);
						strcpy( (*fnp[i+1]).l, b3);
						i+= 1;

						pf =  (*tb[n_f]).pf ;
						uf =  (*tb[n_f]).uf ;
					}

				}
			}

			i++;
			if (i >= uf)
				f_keep = 0;
		}
		while (f_keep);



		/* reviso los allocate */
		i      = 0;
		f_keep = 1;

		do
		{
			pf =  (*tb[n_f]).pf ;
			uf =  (*tb[n_f]).uf ;

			strcpy(b0,(*fnp[i]).l);
			strcpy(b1,pasar_a_minusc ( (*fnp[i]).l) );
	
			f_proceso = 1;
			if (linea_vacia(b0) || es_linea_comentario(b0))
				f_proceso = 0;

			if (f_proceso)
			{
				strcpy(b0,trim_end_f90(b0));
				l2 = strlen(b0);

				if (l2 > 72)
				{
					if (tiene_string(b1,"allocate") && tiene_string(b1,"stv_er") )
					{
						/* puedo armar dos lineas, poniendo stat=stv_er aparte */
						if (l2 > 72 && l2 < 86)
						{
							printf ("FEEa:%03d|%s|\n",l2,b0);

							hacer_lugar_itz(pf,uf,i,1,1);
							memset(b2,0,sizeof(b2));
							memset(b3,0,sizeof(b3));
							arma_dos_lineas_con_alloc_for_v1(b0,b2,b3);

							strcpy( (*fnp[i+0]).l, b2);
							strcpy( (*fnp[i+1]).l, b3);

							i+= 1;
							pf =  (*tb[n_f]).pf ;
							uf =  (*tb[n_f]).uf ;
						}

						/* necesito dos lineas, pero cortar en algun lugar en el medio */
						if (l2 >= 86)
						{
							printf ("FEEb:%03d|%s|\n",l2,b0);

#if 1
							hacer_lugar_itz(pf,uf,i,1,1);

							memset(b2,0,sizeof(b2));
							memset(b3,0,sizeof(b3));
							arma_dos_lineas_con_alloc_for_v2(b0,b2,b3);

							strcpy( (*fnp[i+0]).l, b2);
							strcpy( (*fnp[i+1]).l, b3);

							i+= 1;
							pf =  (*tb[n_f]).pf ;
							uf =  (*tb[n_f]).uf ;
#endif
						}

					}
				}
			}

			i++;
			if (i >= uf)
				f_keep = 0;
		}
		while (f_keep);




		/* reviso los los stat=stv_er que no estan en la linea del allocate  */

		pf =  (*tb[n_f]).pf ;
		uf =  (*tb[n_f]).uf ;

		i      = pf;
		f_keep = 1;

		do
		{

			strcpy(b0,(*fnp[i]).l);
			strcpy(b1,pasar_a_minusc ( (*fnp[i]).l) );
			strcpy(b1,trim_end_f90(b1));
	
			f_proceso = 1;
			if (linea_vacia(b1) || es_linea_comentario(b1))
				f_proceso = 0;

			if (f_proceso)
			{
				l2 = strlen(b1);

				if (l2 > 72)
				{
					if (tiene_string(b1,"stat=stv_er") && !tiene_string(b1,"allocate") )
					{

						/* pos del primer caracter de sentencia ... */
						for (j=7, p1=0, l2=strlen(b0), f1=1; f1 && j<l2; j++)
							if (b0[j] != ' ')
								f1 = 0, p1 = j;

						/* pos del stat=stv_er */
						for (j=0, p2=0, f1=1; f1 && j<l2; j++)
							if (!strncmp(b0+j,",stat=stv_er",12))
								f1 = 0, p2 = j;

						/* si hay comentario ... pos del comentario ... */
						for (j=p2+12, p3=0, f1=1; f1 && j<l2; j++)
							if (b0[j] == '!')
								f1 = 0, p3 = j;


						/* copio la sentencia pura, sin el stat= ... */
						memset(b2,0,sizeof(b2));
						strncpy(b2,b0+p1,p2-p1+1);
printf ("CCC 0 : %2d  |%s|\n",0,b2);

						/* copio si tiene comentarios  ... */
						memset(b3,0,sizeof(b3));
						if (p3)
							strcpy(b3,b0+p3);
						else
							strcpy(b3," ");

printf ("CCC 1 : %2d |%s|\n",1,b3);

						
						/* dos casos 
						 * 1) puedo correr toda la linea ... 
						 * 2) no entra !! tengo que armar una tercera linea !!
						 */

						/* caso 1 ... */
						if (p2-p1+1 <= 46)
						{
							memset(b4,32,sizeof(b4));
							memset(b5,0 ,sizeof(b5));
							strcpy(b5,"     +");
							strncat(b5,b4,50-(p2-p1+1));
							strcat(b5,b2);
							strcat(b5,"stat=stv_er) ");
							if (p3)
								strcat(b5,b3);
						
							strcpy( (*fnp[i+0]).l, b5);
printf ("DDD 2 : %2d |%s|\n",2,b5);

						}
						else
						{
							memset(b4,32,sizeof(b4));
							memset(b5,0,sizeof(b5));
							memset(b6,0,sizeof(b6));
							strcpy(b5,"     +");
							strncat(b5,b4,62-(p2-p1+1));
							strcat(b5,b2);

							strcat(b6,"     +");
							strncat(b6,b4,50);
							strcat(b6,"stat=stv_er) ");
							if (p3)
								strcat(b6,b3);


							pf =  (*tb[n_f]).pf ;
							uf =  (*tb[n_f]).uf ;

							hacer_lugar_itz(pf,uf,i,1,1);

							strcpy( (*fnp[i+0]).l,b5);
							strcpy( (*fnp[i+1]).l,b6);
						
							i+= 1;
printf ("DDD 3 : %2d |%s|\n",3,b0+p1);
printf ("DDD 5 : %2d |%s|\n",5,b5);
printf ("DDD 6 : %2d |%s|\n",6,b6);

						}
						

					}
				}
			}

			i++;
			if (i >= uf)
				f_keep = 0;
		}
		while (f_keep);



		printf ("\n\n");
	}


#endif
/* file tipo 1 */





/* file tipo 2 */
#if 1



	i = 0;
	f_keep = 1;
	f_act  = 1;

	while (f_keep)
	{

/* FFF */
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));
		strcpy(exte_name,e_name(i));

		if (!strncmp(exte_name,"for",3))
			tipo_ext = 1;

		if (!strncmp(exte_name,"f90",3))
			tipo_ext = 2;


	if (tipo_ext == 2)
	{

		pf = (*tb[p_f]).pf;
		uf = (*tb[u_f]).uf;

		strcpy(b0, pasar_a_minusc( (*fnp[i]).l) );
		strcpy(b1,  (*fnp[i]).l );

		strcpy(b0,trim_end_f90(b0));
		strcpy(b0,trim_blanks(b0));

		l2 = strlen(b0);
		l3 = strlen(b1);

		if (l2 > 83)
		{


			if (tiene_string(b0,"stat=stv_er") && tiene_string(b0,"allocate") )
			{

if (gp_debug && w)
{
	mprintf(z,"CCC1 l2:(%3d) b0:|%s|\n",l2,b0);
	mprintf(z,"CCC2 l3:(%3d) b1:|%s|\n",l3,b1);
}

				/* pos del primer caracter de sentencia ... */
				for (j=0, p1=0, l2=strlen(b1), f1=1; f1 && j<l2; j++)
					if (b1[j] != ' ')
						f1 = 0, p1 = j;

				/* pos del stat=stv_er */
				for (j=0, p2=0, f1=1; f1 && j<l2; j++)
					if (!strncmp(b1+j,",stat=stv_er",12))
						f1 = 0, p2 = j;

				/* si hay comentario ... pos del comentario ... */
				for (j=p2+12, p3=0, f1=1; f1 && j<l2; j++)
					if (b1[j] == '!')
						f1 = 0, p3 = j;


				/* cuantos blancos hay al princpio ... */
				for (j=0, p4=0, f1=1; f1 && j<l2; j++)
					if (b1[j] != ' ')
						f1 = 0, p4 = j ;

				/* copio la sentencia pura, sin el stat= ... */
				memset(b2,0,sizeof(b2));
				strncpy(b2,b1+p1,p2-p1+1);

				/* copio si tiene comentarios  ... */
				memset(b3,0,sizeof(b3));
				if (p3)
					strcpy(b3,b1+p3);
				else
					strcpy(b3," ");

if (gp_debug && w)
{
mprintf (z,"CCC3 : %2d |%s|\n",0,b2);
mprintf (z,"CCC4 : %2d |%s|\n",1,b3);
mprintf (z,"CCC5 : p1 %2d p2 %2d p3 %2d p2-p1+1: %2d\n",p1,p2,p3,p2-p1+1);
}

				
				/* 
				 * desdoblo el allocate( ..... ,stat=stv_er)
				 * en ineas ....
				 * aca deberiamos ver la estrtegia
				 * 2 o mas lineas segun la longitud !!
				 *
				 * 1)
				 * len > 83 ... sacamos el stat=stv_er ... (hasta 93... )
				 * ....  2 lineas
				 *
				 * 2)
				 * len > 93 ... sacamos el "      allocate" y "stat=stv_er" .... (hasta 103 )
				 * ..... 3 lineas
				 *
				 * 3) 
				 * len > 103 ... 
				 * habra que dividir en 4 !
				 *       allocate( &
				 *         variable(                 , &
				 *         resto                     , &
				 *         stat=stv_er)
				 * ..... 4 lineas
				 */

				if (l2 > 83 && l2 <= 93)
					tipo_split = 1;
				if (l2 > 93 && l2 <= 103)
					tipo_split = 2;
				if (l2 > 103 )
					tipo_split = 3;

				if (1)
				{
					memset(b4,32,sizeof(b4));
					memset(b5,0,sizeof(b5));
					memset(b6,0,sizeof(b6));
					strncpy(b5,b4,p4);
					strcat(b5,b2);
					strcat(b5," &");

					strncpy(b6,b4,71);
#if 0
					strncat(b6,b4,65-p4);
#endif
					strcat(b6,"stat=stv_er) ");
					if (p3)
						strcat(b6,b3);

					pf =  (*tb[n_f]).pf ;
					uf =  (*tb[n_f]).uf ;

					hacer_lugar_itz(pf,uf,i,1,1);

					strcpy( (*fnp[i+0]).l,b5);
					strcpy( (*fnp[i+1]).l,b6);
				
					i+= 1;
if (gp_debug && w)
{
mprintf (z,"CCC6 : %2d |%s|\n",3,b0+p1);
mprintf (z,"CCC7 : %2d |%s|\n",5,b5);
mprintf (z,"CCC8 : %2d |%s|\n",6,b6);
}

					/* resumen del proceso */
					if (gp_fverbose("d3"))
					{
						printf ("Program  : |%s|\n",prog_name);
						printf ("linea    : |%s|\n",b0+p1);
						printf ("new lin1 : |%s|\n",b5);
						printf ("new lin2 : |%s|\n",b6);
						printf (" ... \n");
					}

				}
				

			}
		}




		if (tiene_string(b0,"check_alloc"))
		{
if (gp_debug && w)
{
mprintf (z,"(1)entre a check_alloc_ok con  ... |%s| \n", (*fnp[i]).l );
mprintf (z,"(2)entre a check_alloc_ok con  ... |%s| \n", b0 );
}
			l2 = strlen(b0);

			if (l2 > 82 )
			{
if (gp_debug && w)
{
mprintf (z,"voy a hacer lugar: pf %d uf %d i %d \n",pf,uf,i);
mprintf (z,"linea: len: %3d |%s|\n", l2, (*fnp[i]).l );
}
				hacer_lugar_itz(pf,uf,i,1,f_act);

				memset(b5,0,sizeof(b5));
				sprintf (b5,"agregado ... check_alloc :%4d",n2);
	
				memset(b2,0,sizeof(b2));
				memset(b3,0,sizeof(b3));

				arma_dos_lineas_con_check(b1,b2,b3);

if (gp_debug && w)
{
mprintf (z,"voy a strcpy 1 i: %d str: |%s| \n",i,(*fnp[i]).l );
}
				strcpy( (*fnp[i+0]).l, b2);
if (gp_debug && w)
{
mprintf (z,"hice  strcpy 1 i: %d str: |%s| \n",i,(*fnp[i]).l );
}

				strcpy( (*fnp[i+1]).l, b3);

				i+= 1;
			}
		}




		} /* if (tipo_ext == 2 ...  */


		i++;
		if (i >= qf_src)
			f_keep = 0;
	}






#endif
/* file tipo 2 */










/* prints ... */
#if 0

if (gp_debug && w)
{
	printf ("Listo las lineas del fuente \n");
	for (i = (*tb[n_f]).pf ; i<= (*tb[n_f]).uf ; i++)
	{
		strcpy(b0,(*fnp[i]).l);

		strcpy(b0,trim_end_f90(b0));

		l2 = strlen(b0);

		printf ("%3d |%s|\n",l2,b0);
	}

	printf ("\n\n");
	
}



if (gp_debug && w)
{
	printf ("Listo las lineas largas del fuente \n");
	for (i = (*tb[n_f]).pf ; i<= (*tb[n_f]).uf ; i++)
	{
		strcpy(b0,(*fnp[i]).l);

		f_proceso = 1;
		if (linea_vacia(b0) || es_linea_comentario(b0))
			f_proceso = 0;

		if (f_proceso)
		{
			strcpy(b0,trim_end_f90(b0));
			l2 = strlen(b0);

			if (l2 > 72)
				printf ("%3d|%s|\n",l2,b0);
		}
	}

	printf ("\n\n");
	
}

#endif
/* prints ... */


#endif
/* desdoblar lineas largas ex9_p1_c */


	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}




/*
 * -----------------------------------------------------------------------------------
 *
 *	sum_tipo 
 *
 * -----------------------------------------------------------------------------------
 */



/*
 * -----------------------------------------------------------------------------------
 *
 *	grabar_a_disco 
 *
 * -----------------------------------------------------------------------------------
 */



int	grabar_a_disco()
{
	static	int	vuelta = 0;
	int	i,j,k;

	vuelta++;

	if (1)
	{

	fprintf (hfou6,"ITER:%5d\n",vuelta);
	fprintf (hfou6,"flag_alloc_ok: %d\n",flag_alloc_ok);
	
	for (i=(*tb[0]).pf ; i<= (*tb[0]).uf; i++)
	{
		fprintf (hfou6,"%05d|%s|\n",i,(*fnp[i]).l);
	}

	fflush(hfou4); fsync(fileno(hfou4));
	fflush(hfou5); fsync(fileno(hfou5));
	fflush(hfou6); fsync(fileno(hfou6));

	}
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_g03
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_g03
 *	agregar check alloc de sentencias con allocate 
 *	tipo 03 for, sl mv no stat

 *	recibe numero de file donde encontro allocate y la linea del allocate
 *
 *	num_f	es el numero de file en la tabla tb
 *
 */

int	chg_alloc_g03(num_f,nf_alloc,add_lines,num_alloc,num_alloc_key,num_alloc_fnd,k_amp_g,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	num_alloc_key;
int	*num_alloc_fnd;
int	*k_amp_g;
int	f_act;
{

	int 	h,i,j,k,k1,k2,k_amp;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	fs_sig,fs1,fs2,fs3;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	src_name[MAXV];
	char	var_name[MAXV];

	char	m0[MSTR];
	char	m1[MSTR];

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_check;	/* linea en la que hay que poner el check_alloc */
	int	linea_stat;	/* linea donde pongo el stat=stv_er */
	int	linea_b;	/* linea desde donde correr */
	int	agrego_lines;	/* lineas a agregar al final x hacer-lugar */
	int	q_vars;		/* cantidad de vars en alloc multi vars ... */

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_g03");
	w = g_dbg(z);

if (gp_debug && w)
{
	mprintf (z,"alloc |%s|\n", (*fnp[nf_alloc]).l );
}


	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	memset(m1,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;


	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;


	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	

		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9010);
if (gp_debug && w)
{
	mprintf (z,"sali de busco_pri_l  pri_l: %d\n",pri_l);
}

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

if (gp_debug && w)
{
	mprintf (z,"sali de busco_ult_l  ult_l: %d\n",ult_l);
}

		/* 
		 * chg_alloc_g03
		 *
		 * a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo n lineas para abajo 
		 * 2) en la ulitma linea del allocate, agregar  stat= ... var de error 
		 * 3) agregar call check_alloc ...
		 *    con variables :
		 *    identificador (string ... nombre de programa con un numero de allocate )
		 *    nombre de la variable ...	
		 *    variable de error 
		 *    opcional ... extra_info ?? 
		 */

		/*
		 * La linea para poner use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 *
		 */

		agrego_lines=0;
		*add_lines = 0;

		linea_check = nf_alloc;
		linea_stat  = nf_alloc;

		k=0;
		k_amp = 0;
		memset(m0,0,MSTR);
		memset(m1,0,MSTR);

		fs_sig = 1;



/* FFF aqui hay que evitar el ! entre las lineas del alloc */

		do
		{


if (gp_debug && w)
{
	mprintf (z,"1\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z,"linea_check %d \n",linea_check);
	mprintf (z," . . . \n");
}

			if (!es_linea_comentario( (*fnp[linea_check+k]).l ))
			{
				strcpy(b2, pasar_a_minusc( (*fnp[linea_check+k]).l) );
				strcpy(b2, trim_beg(b2));
				strcpy(b2, trim_end(b2));
				strcpy(b2, trim_blanks(b2));
				strcat(m0,b2);

				strcpy(b3, (*fnp[linea_check+k]).l );
				strcpy(b3, trim_beg(b3));
				strcpy(b3, trim_end(b3));
				strcpy(b3, trim_blanks(b3));
				strcat(m1,b3);

			}
			k_amp++;
			k++;

			fs1 = tiene_mas( (*fnp[linea_check+k]).l );
			fs2 = es_linea_comentario( (*fnp[linea_check+k]).l );
			fs3 = tiene_mas( (*fnp[linea_check+k+1]).l ) ;

			if (fs1 || (fs2 && fs3))
			{
				fs_sig = 1;
			}
			else
				fs_sig = 0;
		}
		while (fs_sig);

		k2 = k -1;
		k_amp--;

if (gp_debug && w)
{
	mprintf (z,"5\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z," . . . \n");
}

		/* creo que si hay que sacar espacios ... es aca, con m1 */
		strcpy(m1,quitar_blancos_molestos(m1));

		/* calculo cuantas lineas tengo que agregar por desdoblar el allocate */
		q_vars = 200;
		if (!cantidad_de_vars(m1,&q_vars))
			error(9011);

if (gp_debug && w)
{
mprintf (z,"q_vars   : %4d m0: |%s| \n",q_vars,m0);
mprintf (z,"q_vars   : %4d m1: |%s| \n",q_vars,m1);
mprintf (z,"nf_alloc : %4d |%s| \n",nf_alloc,(*fnp[nf_alloc]).l );

}
					
		agrego_lines = q_vars*2-(k2+1)+1;		/* depende de la cant de vars que hay en el alloc !! */
		strcpy(b6,trim_blanks_beg( (*fnp[nf_alloc]).l ));
		grabar_mapa(11,0,b6,nf_alloc,q_vars,0);
		linea_b = nf_alloc+k2+1;

if (gp_debug && w)
{
mprintf (z,"llamo a hacer_lugar_itz  pf: %d  qf_src-1: %d linea_b: %d agrego_lines: %d \n",
	pf,qf_src-1,linea_b,agrego_lines);
mprintf (z,"linea:  |%s| \n", (*fnp[linea_b]).l );
mprintf (z,"-------- \n");
}
		hacer_lugar_itz(pf,qf_src-1,linea_b,agrego_lines,f_act);

		grabar_mapa(10,0," ",num_alloc_key,0,6);

		strcpy(src_name,b_name(nf));
		c5 = n_blanks_beg( (*fnp[nf_alloc]).l );
		memset(blanks,0,sizeof(blanks));
		memset(blanks,32,c5);

if (gp_debug && w)
{
	mprintf (z,"sali de hace lugar  agrego_lines: %d \n", agrego_lines);
}

		/* armo lineas allocate / check_alloc x cada variable en allocate original */
		for (i=0; i<q_vars; i++)
		{
			/* contruyo nuevo allocate */
			strcpy( (*fnp[nf_alloc+2*i]).l    ,blanks);
			strcat( (*fnp[nf_alloc+2*i]).l    ,"allocate(" );
			strcat( (*fnp[nf_alloc+2*i]).l    ,gv_str[i]);
			strcat( (*fnp[nf_alloc+2*i]).l    ,",stat=stv_er)");

			/* contruyo check alloc */
			strcpy(var_name,extract_var_name( (*fnp[nf_alloc+2*i]).l ) );
			memset(b5,0,sizeof(b5));
			sprintf (b5,"(\"%s:%04d\",\"%s\",stv_er)",src_name,num_alloc_key+i,var_name);
			strcpy( (*fnp[nf_alloc+2*i+1]).l  ,blanks);
			strcat( (*fnp[nf_alloc+2*i+1]).l  ,"call check_alloc");
			strcat( (*fnp[nf_alloc+2*i+1]).l  ,b5);

			strcpy(b6,trim_blanks_beg( (*fnp[nf_alloc+2*i]).l ));

			grabar_mapa(12,i+1,b6,0,0,0);

			*num_alloc_fnd = num_alloc_key+i;
	
		}

		strcpy( (*fnp[nf_alloc+2*q_vars]).l,"      ");

		*add_lines = agrego_lines ;
		*k_amp_g = k_amp;
		flag_alloc_ok = 1;
	}

	grabar_mapa(0,0,"-",0,0,5);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_g07
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_g07
 *	agregar check alloc de sentencias con allocate 
 *	tipo 07 for, ml mv no stat  (el mas complicadito)

 *	recibe numero de file donde encontro allocate y la linea del allocate
 *
 *	num_f	es el numero de file en la tabla tb
 *
 */

int	chg_alloc_g07(num_f,nf_alloc,add_lines,num_alloc,num_alloc_key,num_alloc_fnd,k_amp_g,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	num_alloc_key;
int	*num_alloc_fnd;
int	*k_amp_g;
int	f_act;
{

	int 	h,i,j,k,p,k1,k2,k_amp;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	fs_sig,fs1,fs2,fs3;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	src_name[MAXV];
	char	var_name[MAXV];

	char	m0[MSTR];
	char	m1[MSTR];

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_check;	/* linea en la que hay que poner el check_alloc */
	int	linea_stat;	/* linea donde pongo el stat=stv_er */
	int	linea_b;	/* linea desde donde correr */
	int	agrego_lines;	/* lineas a agregar al final x hacer-lugar */
	int	q_vars;		/* cantidad de vars en alloc multi vars ... */

	int	n_lin_x_var[4];	/* tengo que llevar registro, cada var, cuantas lineas ocupa en el alloc !! */

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_g07");
	w = g_dbg(z);

if (gp_debug && w)
{
	mprintf (z,"alloc |%s|\n", (*fnp[nf_alloc]).l );
}


	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	memset(m1,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;


	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;


	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	

		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9010);
if (gp_debug && w)
{
	mprintf (z,"sali de busco_pri_l  pri_l: %d\n",pri_l);
}

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

if (gp_debug && w)
{
	mprintf (z,"sali de busco_ult_l  ult_l: %d\n",ult_l);
}

		/* 
		 * chg_alloc_g07
		 *
		 * a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo n lineas para abajo 
		 * 2) en la ulitma linea del allocate, agregar  stat= ... var de error 
		 * 3) agregar call check_alloc ...
		 *    con variables :
		 *    identificador (string ... nombre de programa con un numero de allocate )
		 *    nombre de la variable ...	
		 *    variable de error 
		 *    opcional ... extra_info ?? 
		 */

		/*
		 * La linea para poner use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 *
		 */

		agrego_lines=0;
		*add_lines = 0;

		linea_check = nf_alloc;
		linea_stat  = nf_alloc;

		k=0;
		k_amp = 0;
		memset(m0,0,MSTR);
		memset(m1,0,MSTR);

		fs_sig = 1;



/* FFF aqui hay que evitar el ! entre las lineas del alloc */

		do
		{


if (gp_debug && w)
{
	mprintf (z,"1\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z,"linea_check %d \n",linea_check);
	mprintf (z," . . . \n");
}

			if (!es_linea_comentario( (*fnp[linea_check+k]).l ))
			{
				strcpy(b2, pasar_a_minusc( (*fnp[linea_check+k]).l) );
				strcpy(b2, trim_beg(b2));
				strcpy(b2, trim_end(b2));
				strcpy(b2, trim_blanks(b2));
				strcat(m0,b2);

				strcpy(b3, (*fnp[linea_check+k]).l );
				strcpy(b3, trim_beg(b3));
				strcpy(b3, trim_end(b3));
				strcpy(b3, trim_blanks(b3));
				strcat(m1,b3);

			}
			k_amp++;
			k++;

			fs1 = tiene_mas( (*fnp[linea_check+k]).l );
			fs2 = es_linea_comentario( (*fnp[linea_check+k]).l );
			fs3 = tiene_mas( (*fnp[linea_check+k+1]).l ) ;

			if (fs1 || (fs2 && fs3))
			{
				fs_sig = 1;
			}
			else
				fs_sig = 0;
		}
		while (fs_sig);

		k2 = k -1;
		k_amp--;

if (gp_debug && w)
{
	mprintf (z,"5\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z," . . . \n");
}


		/* calculo cuantas lineas tengo que agregar por desdoblar el allocate */
		q_vars = 200;
		if (!cantidad_de_vars(m1,&q_vars))
			error(9011);

if (gp_debug && w)
{
mprintf (z,"q_vars   : %4d m0: |%s| \n",q_vars,m0);
mprintf (z,"q_vars   : %4d m1: |%s| \n",q_vars,m1);
mprintf (z,"nf_alloc : %4d |%s| \n",nf_alloc,(*fnp[nf_alloc]).l );

}
					
		/*
		 * atenti !!
		 * si el allocate tiene muchas variables, escritas en 2 o 3 lineas ...
		 * la cuenta de agrego lineas puede dar negativa !!!
		 * en este caso, habria que implementar un "quito_lines! ... 
		 * (queda para otro proyecto )
		 *
		 */

		agrego_lines = q_vars*2-(k2+1)+1;		/* depende de la cant de vars que hay en el alloc !! */
	if (agrego_lines < 0)
	{	
#if 1
		for (p=1; p<=q_vars*2+abs(agrego_lines); p++)
			sprintf ( (*fnp[nf_alloc+p]).l, "!");
#endif
		agrego_lines = 0;
	}

		strcpy(b6,trim_blanks_beg( (*fnp[nf_alloc]).l ));
		grabar_mapa(11,0,b6,nf_alloc,q_vars,0);
		linea_b = nf_alloc+k2+1;

if (gp_debug && w)
{
mprintf (z,"llamo a hacer_lugar_itz  pf: %d  qf_src-1: %d linea_b: %d agrego_lines: %d \n",
	pf,qf_src-1,linea_b,agrego_lines);
mprintf (z,"linea:  |%s| \n", (*fnp[linea_b]).l );
mprintf (z,"-------- \n");
}
		if (agrego_lines > 0)
		{
		hacer_lugar_itz(pf,qf_src-1,linea_b,agrego_lines,f_act);
		}

		grabar_mapa(10,0," ",num_alloc_key,0,6);

		strcpy(src_name,b_name(nf));
		c5 = n_blanks_beg( (*fnp[nf_alloc]).l );
		memset(blanks,0,sizeof(blanks));
		memset(blanks,32,c5);

if (gp_debug && w)
{
	mprintf (z,"sali de hace lugar  agrego_lines: %d \n", agrego_lines);
}

		/* armo lineas allocate / check_alloc x cada variable en allocate original */
		for (i=0; i<q_vars; i++)
		{
			/* contruyo nuevo allocate */
			strcpy( (*fnp[nf_alloc+2*i]).l    ,blanks);
			strcat( (*fnp[nf_alloc+2*i]).l    ,"allocate(" );
			strcat( (*fnp[nf_alloc+2*i]).l    ,gv_str[i]);
			strcat( (*fnp[nf_alloc+2*i]).l    ,",stat=stv_er)");

			/* contruyo check alloc */
			strcpy(var_name,extract_var_name( (*fnp[nf_alloc+2*i]).l ) );
			memset(b5,0,sizeof(b5));
			sprintf (b5,"(\"%s:%04d\",\"%s\",stv_er)",src_name,num_alloc_key+i,var_name);
			strcpy( (*fnp[nf_alloc+2*i+1]).l  ,blanks);
			strcat( (*fnp[nf_alloc+2*i+1]).l  ,"call check_alloc");
			strcat( (*fnp[nf_alloc+2*i+1]).l  ,b5);

			strcpy(b6,trim_blanks_beg( (*fnp[nf_alloc+2*i]).l ));

			grabar_mapa(12,i+1,b6,0,0,0);

			*num_alloc_fnd = num_alloc_key+i;
	
		}

		strcpy( (*fnp[nf_alloc+2*q_vars]).l,"      ");

		*add_lines = agrego_lines ;
		*k_amp_g = k_amp;
		flag_alloc_ok = 1;
	}

	grabar_mapa(0,0,"-",0,0,5);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_g05
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_g05
 *	agregar check alloc de sentencias con allocate 
 *	tipo 05 for, ml sv no stat
 * 
 *	recibe numero de file donde encontro allocate y la linea del allocate
 *
 *
 */

int	chg_alloc_g05(num_f,nf_alloc,add_lines,num_alloc,num_alloc_key,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	num_alloc_key;
int	f_act;
{

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	fs1,fs2,fs3,fs_sig;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	src_name[MAXV];
	char	var_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_check;	/* linea en la que hay que poner el check_alloc */
	int	linea_stat;	/* linea donde pongo el stat=stv_er */
	int	agrego_lines;	/* lineas a agregar al final x hacer-lugar */

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_g05");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	memset(m1,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;


	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;


	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	

if (gp_debug && w)
{
mprintf (z,"entro  a busco_pri_l  pf %d uf %d nf_alloc %d \n",pf,uf,nf_alloc);
}

		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9009);

if (gp_debug && w)
{
mprintf (z,"volvi de busco_pri_l  pf %d uf %d nf_alloc %d \n",pf,uf,nf_alloc);
mprintf (z,"entro  a busco_ult_l  pf %d uf %d nf_alloc %d \n",pf,uf,nf_alloc);
}

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

if (gp_debug && w)
{
mprintf (z,"volvi de busco_pri_l  pf %d uf %d nf_alloc %d ult_l %d\n",pf,uf,nf_alloc,ult_l);
mprintf (z,"ult_l |%s| \n", (*fnp[ult_l]).l );
}

		/* 
		 * chg_alloc_g05
		 *
		 * a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo n lineas para abajo 
		 * 2) en la ulitma linea del allocate, agregar  stat= ... var de error 
		 * 3) agregar call check_alloc ...
		 *    con variables :
		 *    identificador (string ... nombre de programa con un numero de allocate )
		 *    nombre de la variable ...	
		 *    variable de error 
		 *    opcional ... extra_info ?? 
		 */

		/*
		 * La linea para ponder use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 *
		 */

		agrego_lines=2;		/* ojo ! solo sirve para tipo-13 */
		*add_lines = 0;

		linea_check = nf_alloc;
		linea_stat  = nf_alloc;

		k=0;
		memset(m0,0,MSTR);
		memset(m1,0,MSTR);

		/*
		 * m0	contiene la linea pasada a minusculas 
		 * m1	contiene la linea original para mostrar
		 *
		 */

		fs_sig = 1;

if (gp_debug && w)
{
mprintf (z,"entro al while   linea_check %d \n",linea_check);
mprintf (z,"linea_check       |%s| \n", (*fnp[linea_check]).l );
}

		do
		{



if (gp_debug && w)
{
	mprintf (z,"en el while ... (1)\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z,"linea_check %d \n",linea_check);
	mprintf (z,"linea_check+k : |%s| \n", (*fnp[linea_check+k]).l );
	mprintf (z,"fs1 %d \n",fs1);
	mprintf (z,"fs2 %d \n",fs2);
	mprintf (z,"fs3 %d \n",fs3);
	mprintf (z," . . . \n");
}



			if (!es_linea_comentario( (*fnp[linea_check+k]).l  ))
			{
if (gp_debug && w)
{
	mprintf (z,"entre a if (es_linea_comentario ... \n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z,"linea_check %d \n",linea_check);
	mprintf (z,"linea_check+k : |%s| \n", (*fnp[linea_check+k]).l );
	mprintf (z," . . . \n");
}
				strcpy(b2, pasar_a_minusc( (*fnp[linea_check+k]).l) );
				strcpy(b2,chanchada_for(b2));
				strcpy(b2, trim_beg_f90(b2));
				strcpy(b2, trim_end_f90(b2));
				strcpy(b2, trim_blanks(b2));
				strcat(m0,b2);

				strcpy(b3, (*fnp[linea_check+k]).l );
				strcpy(b3,chanchada_for(b3));
				strcpy(b3, trim_beg_f90(b3));
				strcpy(b3, trim_end_f90(b3));
				strcpy(b3, trim_blanks(b3));
				strcat(m1,b3);
			}
			k++;

			fs1 = tiene_mas( (*fnp[linea_check+k]).l );
			fs2 = es_linea_comentario( (*fnp[linea_check+k]).l );
			fs3 = tiene_mas( (*fnp[linea_check+k+1]).l ) ;


			if ( fs1 || (fs2 && fs3))
			{
				fs_sig = 1;
			}
			else
				fs_sig = 0;

if (gp_debug && w)
{
	mprintf (z,"pie de while ... (2)\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z," . . . \n");
}

		}
		while (fs_sig);

		k2 = k-1;

if (gp_debug && w)
{
mprintf (z,"alloc: 3 m0: |%s|\n",m0);
mprintf (z,"alloc: 3 m1: |%s|\n",m1);
}

		agrego_lines = 2;
		linea_stat   = nf_alloc + k2;
		linea_check  = linea_stat + 1;

		grabar_mapa(7,0,(*fnp[linea_check]).l,lne(linea_check),lne(pri_l),lne(ult_l));

		hacer_lugar_itz(pf,qf_src-1,linea_check,agrego_lines,f_act);

		c5 = n_blanks_beg( (*fnp[nf_alloc]).l );
		memset(blanks,0,sizeof(blanks));
		memset(blanks,32,c5);
	
		grabar_mapa(10,0," ",num_alloc_key,0,6);

if (gp_debug && w)
{
mprintf (z,"voy a extract name con m1: |%s| \n",m1);
}
		strcpy(var_name,extract_var_name( m1 ) );
		strcpy(src_name,b_name(nf));

		memset(b5,0,sizeof(b5));
		sprintf (b5,"(\"%s:%04d\",\"%s\",stv_er)",src_name,num_alloc_key,var_name);
		strcpy( (*fnp[linea_check]).l  ,blanks);
		strcat( (*fnp[linea_check]).l  ,"call check_alloc");
		strcat( (*fnp[linea_check]).l  ,b5);

#if 1
		strcpy( (*fnp[linea_check+1]).l,"     ");
#endif

		/* aca tengo que modificar la linea nf_alloc ... 
	  	 * esto va a ser un lio cuando tenga que meter 
		 * lineas compuestas ! 
		 */

		/* para version de src 'for' */
		agregar_stv_2 (linea_stat);

		*add_lines = agrego_lines;
		flag_alloc_ok = 1;

	}

	
	grabar_mapa(0,0,"-",0,0,4);
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_g09
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_g09
 *	agregar check alloc de sentencias con allocate 
 *	tipo-09 f90 sl sv
 *	tipo-11 f90 sl mv
 *
 *	recibe numero de file donde encontro allocate y la linea del allocate
 *
 *	Atenti:
 *	esto corrije un solo allocate x vez !!!
 */

int	chg_alloc_g09(num_f,nf_alloc,add_lines,num_alloc,num_alloc_key,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	num_alloc_key;
int	f_act;
{
	static	int	f_msgmmsb6 = 0;

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	src_name[MAXV];
	char	var_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_check;	/* linea en la que hay que poner el check_alloc */
	int	agrego_lines;	/* lineas a agregar al final x hacer-lugar */

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_g09");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;

	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;

	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;

	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	
		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9008);

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

		/* a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo 1 linea para abajo, y agregar el use allocate_vars.f90 
		 * 2) en la linea del allocate, agregar  stat= ... var de error 
		 * 3) correr todo 1 linea para abajo, y agregar call check_alloc ...
		 *    con variables :
		 *    identificador (string ... nombre de programa con un numero de allocate )
		 *    nombre de la variable ...	
		 *    variable de error 
		 *    opcional ... extra_info ?? 
		 */


		/*
		 * La linea para ponder use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 *
		 */

		linea_check = nf_alloc + 1;
		agrego_lines=2;
		*add_lines = 0;


		grabar_mapa(7,0,(*fnp[linea_check]).l,lne(linea_check),lne(pri_l),lne(ult_l));

		hacer_lugar_itz(pf,qf_src-1,linea_check,agrego_lines,f_act);

		c5 = n_blanks_beg( (*fnp[nf_alloc]).l );


		/* casos particulares */
		if (!strncmp(prog_name,"msgmmsb6.for",12))
		{
			if (f_msgmmsb6 == 0 && !strncmp( (*fnp[nf_alloc]).l, "     +                          ALLOCATE(SUBs_LINKED_TO_PARENT(-1:1024))",72))
			{
				f_msgmmsb6 = 1;
				c5 += 4;
			}
		}

		/* saco los blancos molestos */
		memset(m1,0,sizeof(m1));
		strcpy(m1, (*fnp[nf_alloc]).l);
		strcpy(m1, quitar_blancos_molestos(m1));
		strcpy( (*fnp[nf_alloc]).l, m1);



		memset(blanks,0,sizeof(blanks));
		memset(blanks,32,c5);

		grabar_mapa(10,0," ",num_alloc_key,0,6);
	
		strcpy(var_name,extract_var_name( (*fnp[nf_alloc]).l ) );
		strcpy(src_name,b_name(nf));
if (gp_debug && w)
{
	mprintf(z,"nf: %3d src_name: |%s|\n",nf,src_name);
}
		memset(b5,0,sizeof(b5));
		sprintf (b5,"(\"%s:%04d\",\"%s\",stv_er)",src_name,num_alloc_key,var_name);
		strcpy( (*fnp[linea_check]).l  ,blanks);
		strcat( (*fnp[linea_check]).l  ,"call check_alloc");
		strcat( (*fnp[linea_check]).l  ,b5);

		strcpy( (*fnp[linea_check+1]).l,"     ");

		/* aca tengo que modificar la linea nf_alloc ... 
	  	 * esto va a ser un lio cuando tenga que meter 
		 * lineas compuestas ! 
		 */

		agregar_stv (nf_alloc);

		*add_lines = agrego_lines;
		flag_alloc_ok = 1;
	}

	grabar_mapa(0,0,"-",0,0,1);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_g02
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_g02
 *	agregar check alloc de sentencias con allocate 
 *	tipo 13 f90 ml sv
 * 
 *	recibe numero de file donde encontro allocate y la linea del allocate
 *
 *	Atenti:
 *	esto corrije un solo allocate x vez !!!
 *
 */

int	chg_alloc_g02(num_f,nf_alloc,add_lines,num_alloc,num_alloc_key,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	num_alloc_key;
int	f_act;
{

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	fs_sig,fs1,fs2,fs3;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	src_name[MAXV];
	char	var_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_check;	/* linea en la que hay que poner el check_alloc */
	int	linea_stat;	/* linea donde pongo el stat=stv_er */
	int	agrego_lines;	/* lineas a agregar al final x hacer-lugar */

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_g02");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	memset(m1,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;


	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;


	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	

if (gp_debug && w)
{
mprintf (z,"entro  a busco_pri_l  pf %d uf %d nf_alloc %d \n",pf,uf,nf_alloc);
}

		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9009);

if (gp_debug && w)
{
mprintf (z,"volvi de busco_pri_l  pf %d uf %d nf_alloc %d \n",pf,uf,nf_alloc);
mprintf (z,"entro  a busco_ult_l  pf %d uf %d nf_alloc %d \n",pf,uf,nf_alloc);
}

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

if (gp_debug && w)
{
mprintf (z,"volvi de busco_pri_l  pf %d uf %d nf_alloc %d ult_l %d\n",pf,uf,nf_alloc,ult_l);
mprintf (z,"ult_l |%s| \n", (*fnp[ult_l]).l );
}

		/* 
		 * chg_alloc_g02
		 *
		 * a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo n lineas para abajo 
		 * 2) en la ulitma linea del allocate, agregar  stat= ... var de error 
		 * 3) agregar call check_alloc ...
		 *    con variables :
		 *    identificador (string ... nombre de programa con un numero de allocate )
		 *    nombre de la variable ...	
		 *    variable de error 
		 *    opcional ... extra_info ?? 
		 */

		/*
		 * La linea para ponder use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 *
		 */

		agrego_lines=2;		/* ojo ! solo sirve para tipo-13 */
		*add_lines = 0;

		linea_check = nf_alloc;
		linea_stat  = nf_alloc;

		k=0;
		memset(m0,0,MSTR);
		memset(m1,0,MSTR);

		/*
		 * m0	contiene la linea pasada a minusculas 
		 * m1	contiene la linea original para mostrar
		 *
		 */

		fs_sig = 1;

if (gp_debug && w)
{
mprintf (z,"entro al while   linea_check %d \n",linea_check);
mprintf (z,"linea_check       |%s| \n", (*fnp[linea_check]).l );
}

		while (fs_sig)
		{

			fs1 = tiene_amper( (*fnp[linea_check+k]).l );
			fs2 = es_linea_comentario( (*fnp[linea_check+k]).l );
			fs3 = tiene_amper( (*fnp[linea_check+k+1]).l ) ;


if (gp_debug && w)
{
	mprintf (z,"en el while ... (1)\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z,"linea_check %d \n",linea_check);
	mprintf (z,"linea_check+k : |%s| \n", (*fnp[linea_check+k]).l );
	mprintf (z,"fs1 %d \n",fs1);
	mprintf (z,"fs2 %d \n",fs2);
	mprintf (z,"fs3 %d \n",fs3);
	mprintf (z," . . . \n");
}

			if ( fs1 || (fs2 && fs3))
			{


			if (!es_linea_comentario( (*fnp[linea_check+k]).l  ))
			{
if (gp_debug && w)
{
	mprintf (z,"entre a if (es_linea_comentario ... \n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z,"linea_check %d \n",linea_check);
	mprintf (z,"linea_check+k : |%s| \n", (*fnp[linea_check+k]).l );
	mprintf (z," . . . \n");
}
				strcpy(b2, pasar_a_minusc( (*fnp[linea_check+k]).l) );
				strcpy(b2, trim_beg_f90(b2));
				strcpy(b2, trim_end_f90(b2));
				strcpy(b2, trim_blanks(b2));
				strcat(m0,b2);
				strcpy(m0,chanchada(m0));

				strcpy(b3, (*fnp[linea_check+k]).l );
				strcpy(b3, trim_beg_f90(b3));
				strcpy(b3, trim_end_f90(b3));
				strcpy(b3, trim_blanks(b3));
				strcat(m1,b3);
				strcpy(m1,chanchada(m1));
			}
/* OJO.... me parece que este k++ tiene que ser parte del if anterior */
			k++;

			}
			else
				fs_sig = 0;

if (gp_debug && w)
{
	mprintf (z,"pie de while ... (2)\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z," . . . \n");
}

		}

		strcpy(b2, pasar_a_minusc( (*fnp[linea_check+k]).l) );
		strcpy(b2, trim_beg_f90(b2));
		strcpy(b2, trim_end_f90(b2));
		strcpy(b2, trim_blanks(b2));
		strcat(m0,b2);
		strcpy(m0,chanchada(m0));

		strcpy(b3, (*fnp[linea_check+k]).l );
		strcpy(b3, trim_beg_f90(b3));
		strcpy(b3, trim_end_f90(b3));
		strcpy(b3, trim_blanks(b3));
		strcat(m1,b3);
		strcpy(m1,chanchada(m1));


		/*
		 * combinaciones:
		 * 1) agrego_lines = k y saco el strcpy al final de "    "
		 * 2) agrego_lines = k+1 .. y dejo el strcpy .. esto deja una linea vacia desp de cada allocte
		 *
		 */

#if 0
		agrego_lines = k+1;
#endif
#if 0
		agrego_lines = k;
#endif
		agrego_lines = 2;
		linea_stat   = nf_alloc + k;
		linea_check  = linea_stat + 1;

		grabar_mapa(7,0,(*fnp[linea_check]).l,lne(linea_check),lne(pri_l),lne(ult_l));

		hacer_lugar_itz(pf,qf_src-1,linea_check,agrego_lines,f_act);

		c5 = n_blanks_beg( (*fnp[nf_alloc]).l );
		memset(blanks,0,sizeof(blanks));
		memset(blanks,32,c5);
	
		grabar_mapa(10,0," ",num_alloc_key,0,6);

		strcpy(var_name,extract_var_name( m1 ) );
		strcpy(src_name,b_name(nf));

		memset(b5,0,sizeof(b5));
		sprintf (b5,"(\"%s:%04d\",\"%s\",stv_er)",src_name,num_alloc_key,var_name);
		strcpy( (*fnp[linea_check]).l  ,blanks);
		strcat( (*fnp[linea_check]).l  ,"call check_alloc");
		strcat( (*fnp[linea_check]).l  ,b5);

#if 1
		strcpy( (*fnp[linea_check+1]).l,"     ");
#endif

		/* aca tengo que modificar la linea nf_alloc ... 
	  	 * esto va a ser un lio cuando tenga que meter 
		 * lineas compuestas ! 
		 */

		agregar_stv_2 (linea_stat);

		*add_lines = agrego_lines;
		flag_alloc_ok = 1;

	}

	
	grabar_mapa(0,0,"-",0,0,4);
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_g15
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_g15
 *	agregar check alloc de sentencias con allocate tipo 15 
 *	recibe numero de file donde encontro allocate y la linea del allocate
 *
 *	Atenti:
 *	esto corrije un solo allocate x vez !!!
 *
 *	g15 f90 ... grupo para ml,mv ....  el mas complejito !
 *
 *	num_f	es el numero de file en la tabla tb
 *
 */

int	chg_alloc_g15(num_f,nf_alloc,add_lines,num_alloc,num_alloc_key,num_alloc_fnd,k_amp_g,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	num_alloc_key;
int	*num_alloc_fnd;
int	*k_amp_g;
int	f_act;
{

	int 	h,i,j,k,k1,k2,k_amp;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	fs_sig,fs1,fs2,fs3;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	src_name[MAXV];
	char	var_name[MAXV];

	char	m0[MSTR];
	char	m1[MSTR];

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	b6[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_check;	/* linea en la que hay que poner el check_alloc */
	int	linea_stat;	/* linea donde pongo el stat=stv_er */
	int	linea_b;	/* linea desde donde correr */
	int	agrego_lines;	/* lineas a agregar al final x hacer-lugar */
	int	q_vars;		/* cantidad de vars en alloc multi vars ... */

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_g15");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	memset(m1,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;


	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;


	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	

		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9010);

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

		/* 
		 * chg_alloc_g15
		 *
		 * a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo n lineas para abajo 
		 * 2) en la ulitma linea del allocate, agregar  stat= ... var de error 
		 * 3) agregar call check_alloc ...
		 *    con variables :
		 *    identificador (string ... nombre de programa con un numero de allocate )
		 *    nombre de la variable ...	
		 *    variable de error 
		 *    opcional ... extra_info ?? 
		 */

		/*
		 * La linea para poner use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 *
		 */

		agrego_lines=0;
		*add_lines = 0;

		linea_check = nf_alloc;
		linea_stat  = nf_alloc;

		k=0;
		k_amp = 0;
		memset(m0,0,MSTR);
		memset(m1,0,MSTR);

		fs_sig = 1;


/* FFF aqui hay que evitar el ! entre las lineas del alloc */
#if 1
		while (fs_sig)
		{

			fs1 = tiene_amper( (*fnp[linea_check+k]).l );
			fs2 = es_linea_comentario( (*fnp[linea_check+k]).l );
			fs3 = tiene_amper( (*fnp[linea_check+k+1]).l ) ;

			if (fs1 || (fs2 && fs3))
			{

if (gp_debug && w)
{
	mprintf (z,"1\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z,"linea_check %d \n",linea_check);
	mprintf (z," . . . \n");
}

				if (!es_linea_comentario( (*fnp[linea_check+k]).l ))
				{
					strcpy(b2, pasar_a_minusc( (*fnp[linea_check+k]).l) );
					strcpy(b2, trim_beg_f90(b2));
					strcpy(b2, trim_end_f90(b2));
					strcpy(b2, trim_blanks(b2));
					strcat(m0,b2);
	
					strcpy(b3, (*fnp[linea_check+k]).l );
					strcpy(b3, trim_beg_f90(b3));
					strcpy(b3, trim_end_f90(b3));
					strcpy(b3, trim_blanks(b3));
					strcat(m1,b3);
	
				}
				k_amp++;
				k++;
			}
			else
				fs_sig = 0;
		}

		strcpy(b2, pasar_a_minusc( (*fnp[linea_check+k]).l) );
		strcpy(b2, trim_beg_f90(b2));
		strcpy(b2, trim_end_f90(b2));
		strcpy(b2, trim_blanks(b2));
		strcat(m0,b2);

		strcpy(b3, (*fnp[linea_check+k]).l );
		strcpy(b3, trim_beg_f90(b3));
		strcpy(b3, trim_end_f90(b3));
		strcpy(b3, trim_blanks(b3));
		strcat(m1,b3);

if (gp_debug && w)
{
	mprintf (z,"5\n");
	mprintf (z,"k: %d  |%s| \n",k,m0);
	mprintf (z,"k: %d  |%s| \n",k,m1);
	mprintf (z," . . . \n");
}

#endif

		/* creo que si hay que sacar espacios ... es aca, con m1 */
		strcpy(m1,quitar_blancos_molestos(m1));

		/* calculo cuantas lineas tengo que agregar por desdoblar el allocate */
		q_vars = 200;
		if (!cantidad_de_vars(m1,&q_vars))
			error(9011);

if (gp_debug && w)
{
mprintf (z,"q_vars   : %4d m0: |%s| \n",q_vars,m0);
mprintf (z,"q_vars   : %4d m1: |%s| \n",q_vars,m1);
mprintf (z,"nf_alloc : %4d |%s| \n",nf_alloc,(*fnp[nf_alloc]).l );

}
					
		/* atencion con esto !!
		 * si la cantidad de linas que ocupa el allocate original 
		 * es mayor a la cuenta que da el espacio necesario para
		 * re escribirlos, entonces no hay que hacer lugar, 
		 * sino mas bien, habria que "subir" el codigo !!!!
		 */

		if ( (k+1) >= q_vars*2+1 )
		{
			/* me sobran lineas !!! tengo que borrar parte del codigo del allocate !! */
			agrego_lines = 0;
			
			for (i=1; i<= k; i++)
			{
				strcpy( (*fnp[linea_check+i]).l, "vacio  ");
			}
		}
		else
		{
			agrego_lines = q_vars*2-(k+1)+1;	/* depende de la cant de vars que hay en el alloc !! */
		}

		strcpy(b6,trim_blanks_beg( (*fnp[nf_alloc]).l ));

		grabar_mapa(11,0,b6,nf_alloc,q_vars,0);

		linea_b = nf_alloc+k+1;

if (gp_debug && w)
{
mprintf (z,"llamo a hacer_lugar_itz  pf: %d  qf_src-1: %d linea_b: %d agrego_lines: %d \n",
	pf,qf_src-1,linea_b,agrego_lines);
mprintf (z,"linea:  |%s| \n", (*fnp[linea_b]).l );
mprintf (z,"-------- \n");
}
		if (agrego_lines )
		{
			hacer_lugar_itz(pf,qf_src-1,linea_b,agrego_lines,f_act);
		}

		grabar_mapa(10,0," ",num_alloc_key,0,6);

		strcpy(src_name,b_name(nf));
		c5 = n_blanks_beg( (*fnp[nf_alloc]).l );
		memset(blanks,0,sizeof(blanks));
		memset(blanks,32,c5);

if (gp_debug && w)
{
mprintf (z,"nro de blancos c5:  %d  |%s|\n",c5,(*fnp[nf_alloc]).l );
mprintf (z,"- - - \n");
}

#if 1
		/* armo lineas allocate / check_alloc x cada variable en allocate original */
		for (i=0; i<q_vars; i++)
		{
			/* contruyo nuevo allocate */
			strcpy( (*fnp[nf_alloc+2*i]).l    ,blanks);
			strcat( (*fnp[nf_alloc+2*i]).l    ,"allocate(" );
			strcat( (*fnp[nf_alloc+2*i]).l    ,gv_str[i]);
			strcat( (*fnp[nf_alloc+2*i]).l    ,",stat=stv_er)");

			/* contruyo check alloc */
			strcpy(var_name,extract_var_name( (*fnp[nf_alloc+2*i]).l ) );
			memset(b5,0,sizeof(b5));
			sprintf (b5,"(\"%s:%04d\",\"%s\",stv_er)",src_name,num_alloc_key+i,var_name);
			strcpy( (*fnp[nf_alloc+2*i+1]).l  ,blanks);
			strcat( (*fnp[nf_alloc+2*i+1]).l  ,"call check_alloc");
			strcat( (*fnp[nf_alloc+2*i+1]).l  ,b5);

			strcpy(b6,trim_blanks_beg( (*fnp[nf_alloc+2*i]).l ));
			grabar_mapa(12,i+1,b6,0,0,0);

			*num_alloc_fnd = num_alloc_key+i;
		}
	
#endif
		strcpy( (*fnp[nf_alloc+2*q_vars]).l,"      ");

		*add_lines = agrego_lines ;
		*k_amp_g = k_amp;
		flag_alloc_ok = 1;
	}

	
	grabar_mapa(0,0,"-",0,0,5);
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	extract_var_name
 *
 *
 * -----------------------------------------------------------------------------------
 */



char	*extract_var_name(s)
char	*s;
{
	char	b0[MSTR];
	char	b1[MSTR];
	static	char	b2[MAXV];

	int	f_res;
	int	f_sigo;
	int	f_open_par;
	int	f_opt;
	int	caso;
	int	p1,p2,p3;
	int	i,j,k;
	int	l1,l2;
	int	c1,c2,c3;
	int	f1;
	int	st1,st2,st3,st4;

	char	z[MAXV];
	int	w;
	sprintf (z,"extract_var_name");
	w = g_dbg(z);

	/* copio string para trabajar local */
	strcpy(b0,pasar_a_minusc(s));
	strcpy(b1,s);

	l2 = strlen(b0);

	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	/* largo definitivo de la sentencia */
	l2 = strlen(b0);
	

	f_res = 0;
	p1    = 0;
	p2    = l2 -1;

	st1 = 0;
	st2 = 0;
	st3 = 0;

		
	/* la primer palabra tiene que ser allocate */
	st1 = 0;
	for (i=0, f_sigo=1 ; f_sigo && i<=p2; i++)
	{	if (b0[i] == ' ' || b0[i] == '\t')
			p1++;
		else
		{	
			if (!strncmp(b0+i,"allocate",8))
			{
				f_sigo = 0;
				st1 = 1;
				p1 = i+8;
			}
		}
	}
	
if (gp_debug && w)
{
mprintf (z,"paso 1 - rimer palabra es allocate          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}


	/* puede haber blancos entre allocate y ( ... los quito */
	while (b0[p1] == ' ')
	{
		l2 = strlen(b0)-1;
		for (k=p1; k<l2; k++)
		{	b0[k] = b0[k+1];
			b1[k] = b1[k+1];
		}
		b0[l2]=0;
		b1[l2]=0;
		p2--;
	}


if (gp_debug && w)
{
mprintf (z,"paso 2 - si hay blancos corro la linea          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}

	/* el ultimo caracter salvo blancos, es un parentesis que cierra */
	st2 = 0;
	l2 = strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i-- )
		if (b0[i] == ' ')
			p2--;
		else
		{
			if (b0[i] == ')')
			{
				f_sigo = 0;
				st2 = 1;
				p2  = i;
			}
			else
			{
				f_sigo = 0;
			}
		}

if (gp_debug && w)
{
mprintf (z,"paso 3 - el ultimo caracter salvo blanco es )           \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	/* extract_var_name : caracteres especificos de allocate entre ( y ) */
	st3 = 1;
	for (i=p1; i<=p2; i++)
	{
		if (!((b0[i] >= 'a' && b0[i] <= 'z') || 
                      (b0[i] >= '0' && b0[i] <= '9') || 
		      b0[i] == '=' ||						/* por lineas con el bendito stat= */
                      b0[i] == ' ' || b0[i] == ':' || b0[i] == ',' || 
                      b0[i] == '-' || b0[i] == '(' || b0[i] == ')' || 
	              b0[i] == '_' || b0[i] == '*' || b0[i] == '+'    )  )
		{
			st3 = 0;
		}
	}

	
if (gp_debug && w)
{
mprintf (z,"paso 4 - caracteres especificos entre ( y )          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}



	f_sigo = 1;
	c2  = 0;
	c3  = 0;
	st4 = 0;
	i   = p1;

	while (f_sigo)
	{
		f_opt = 1;

		if (f_opt && c2 == 0 && b0[i] == '(' )
		{
			c2 = 1;
			f_opt = 0;
		}

		if (f_opt && c2 >= 1 && b0[i] == '(' )
		{
			c2++;
			f_opt = 0;
		}

		if (f_opt && c2 > 1 && b0[i] == ')' )
		{
			c2--;
			f_opt = 0;
		}

		if (f_opt && c2 == 1 && b0[i] == ',')
		{
			c3++;
			f_opt = 0;
		}
 
		i++;

		if (i> p2)
		{
			f_sigo = 0;
		}

	}

	if (c3)
	{
		st4 = 1;
		
		if (tiene_stat(b0) && c3 == 1 )
			st4 = 0;
	}

if (gp_debug && w)
{
mprintf (z,"paso 5 - hay comas sin estar entre    ( y )  \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d,%d| \n",st1,st2,st3,st4);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	if (st1 && st2 && st3 && st4)
		f_res = 1;

	/*
	 * ver despues como manejar los diferentes casos !!
	 * caso 1 - allocate en una linea, con una sola variable 
	 */
 	
	caso = 1;
	
	if (f_res = 1 && caso == 1)
	{

if (gp_debug && w)
{
mprintf (z,"p1: %d %c p2: %d %c\n",p1,b0[p1],p2,b0[p2]);
mprintf (z,"b0: |%s| \n",b0);
}

		memset(b2,0,sizeof(b2));

		/* en p1,p2 estan los ( ) */
		for (i=p1+1, p3=0, f_sigo=1, f_res = 0; f_sigo && i<= p2-1; i++)
		{
			if ( (b0[i] >= 'a' && b0[i] <= 'z') || (b0[i] >= '0' && b0[i] <= '9') ||
                              b0[i] == '_' )
			{
				b2[p3] = b1[i];
				p3++;
			}
			else
				f_sigo=0, f_res=1;
if (gp_debug && w)
{
mprintf (z,"p1 p2 p3, %2d %2d %2d i %2d b1: |%s| \n",p1,p2,p3,i,b1);
}
		}
	}
	else
		error(9004);		
	
if (gp_debug && w)
{
mprintf (z,"paso 5           \n");
mprintf (z,"f_res %d var: b0: |%s| \n",f_res,b0);
}

if (gp_debug && w)
{
mprintf (z,"var ext: b2: |%s| \n",b2);
mprintf (z,"salgo ... \n");
}


	return (b2);

}


/*
 * -----------------------------------------------------------------------------------
 *
 *	agregar_stv
 *
 *
 * -----------------------------------------------------------------------------------
 */

int	agregar_stv(nf_alloc)
int	nf_alloc;
{
	char	b0[MSTR];
	char	b1[MSTR];
	char	b5[MSTR];
	char	b2[MAXV];

	char	cp_str[16];

	int	f_res;
	int	f_sigo;
	int	f_open_par;
	int	f_opt;

	int	caso;
	int	p1,p2,p3;
	int	i,j,k;
	int	l1,l2,l3,l4;
	int	c1,c2,c3;

	int	f1;

	int	st1,st2,st3,st4;

	char	z[MAXV];
	int	w;
	sprintf (z,"agregar_stv");
	w = g_dbg(z);


	/* copio string para trabajar local */
	strcpy(b0,pasar_a_minusc( (*fnp[nf_alloc]).l ));
	strcpy(b1, (*fnp[nf_alloc]).l );

	l2 = strlen(b0);


	strcpy(cp_str,",stat=stv_er");
	l3 = strlen(cp_str);

	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	/* largo definitivo de la sentencia */
	l2 = strlen(b0);
	

	f_res = 0;
	p1    = 0;
	p2    = l2 -1;

	st1 = 0;
	st2 = 0;
	st3 = 0;

		
	/* la primer palabra tiene que ser allocate */
	st1 = 0;
	for (i=0, f_sigo=1 ; f_sigo && i<=p2; i++)
	{	if (b0[i] == ' ' || b0[i] == '\t')
			p1++;
		else
		{	
			if (!strncmp(b0+i,"allocate",8))
			{
				f_sigo = 0;
				st1 = 1;
				p1 = i+8;
			}
		}
	}
	
if (gp_debug && w)
{
mprintf (z,"paso 1 - primer palabra es allocate          \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}


	/* puede haber blancos entre allocate y ( ... los quito */
	while (b0[p1] == ' ')
	{
		l2 = strlen(b0)-1;
		for (k=p1; k<l2; k++)
		{	b0[k] = b0[k+1];
			b1[k] = b1[k+1];
		}
		b0[l2]=0;
		b1[l2]=0;
		p2--;
	}


if (gp_debug && w)
{
mprintf (z,"paso 2 - si hay blancos corro la linea          \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}

	/* el ultimo caracter salvo blancos, es un parentesis que cierra */
	st2 = 0;
	l2 = strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i-- )
		if (b0[i] == ' ')
			p2--;
		else
		{
			if (b0[i] == ')')
			{
				f_sigo = 0;
				st2 = 1;
				p2  = i;
			}
			else
			{
				f_sigo = 0;
			}
		}

if (gp_debug && w)
{
mprintf (z,"paso 3 - el ultimo caracter salvo blanco es )           \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	/* agregar_stv : caracteres especificos de allocate entre ( y ) */
	st3 = 1;
	for (i=p1; i<=p2; i++)
	{
		if (!((b0[i] >= 'a' && b0[i] <= 'z') || 
                      (b0[i] >= '0' && b0[i] <= '9') || 
		      b0[i] == '=' ||						/* por lineas con el bendito stat= */
                      b0[i] == ' ' || b0[i] == ':' || b0[i] == ',' || 
                      b0[i] == '-' || b0[i] == '(' || b0[i] == ')' || 
	              b0[i] == '_' || b0[i] == '*' || b0[i] == '+' ||
		      b0[i] == '%'   )  )					/* por la bendita estructura   texto%texto ... */
		{
			st3 = 0;
		}
	}

	
if (gp_debug && w)
{
mprintf (z,"paso 4 - caracteres especificos entre ( y )          \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}

	f_sigo = 1;
	c2  = 0;
	c3  = 0;
	st4 = 0;
	i   = p1;

	while (f_sigo)
	{
		f_opt = 1;

		if (f_opt && c2 == 0 && b0[i] == '(' )
		{
			c2 = 1;
			f_opt = 0;
		}

		if (f_opt && c2 >= 1 && b0[i] == '(' )
		{
			c2++;
			f_opt = 0;
		}

		if (f_opt && c2 > 1 && b0[i] == ')' )
		{
			c2--;
			f_opt = 0;
		}

		if (f_opt && c2 == 1 && b0[i] == ',')
		{
			c3++;
			f_opt = 0;
		}
 
		i++;

		if (i> p2)
		{
			f_sigo = 0;
		}

	}

	if (c3)
	{
		st4 = 1;
		
		if (tiene_stat(b0) && c3 == 1 )
			st4 = 0;
	}

if (gp_debug && w)
{
mprintf (z,"paso 5 - hay comas sin estar entre    ( y )  \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d,%d| \n",st1,st2,st3,st4);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	if (st1 && st2 && st3 && st4)
		f_res = 1;

	/*
	 * ver despues como manejar los diferentes casos !!
	 * caso 1 - allocate en una linea, con una sola variable 
	 *
	 * STAT=STV_ER
	 */
 	
	caso = 1;
	
	if (f_res = 1 && caso == 1)
	{

if (gp_debug && w)
{
mprintf (z,"p1: %d %c p2: %d %c\n",p1,b0[p1],p2,b0[p2]);
mprintf (z,"b0: |%s| \n",b0);
mprintf (z,"b1: |%s| \n",b1);
}

		
		l4 = strlen(b0);
		memset(b2,0,sizeof(b2));
		memset(b5,0,sizeof(b5));


		strcpy(b5,b1);
		b5[p2]=0;
		strcat(b5,cp_str);
		strcat(b5,b1+p2);

if (gp_debug && w)
{
mprintf (z,"p1 p2 p3, %2d %2d %2d i %2d  \n",p1,p2,p3,i);
mprintf (z,"b0: |%s| \n",b0);
mprintf (z,"b1: |%s| \n",b1);
mprintf (z,"b5: |%s| \n",b5);
}

		strcpy( (*fnp[nf_alloc]).l, b5);

#if 0
		/* en p1,p2 estan los ( ) */
		for (i=p1+1, p3=0, f_sigo=1, f_res = 0; f_sigo && i<= p2-1; i++)
		{
			if ( (b0[i] >= 'a' && b0[i] <= 'z') || (b0[i] >= '0' && b0[i] <= '9') ||
                              b0[i] == '_' )
			{
				b2[p3] = b1[i];
				p3++;
			}
			else
				f_sigo=0, f_res=1;
		}
#endif
	}
	else
		error(9013);		
	

if (gp_debug && w)
{
mprintf (z,"paso 5           \n");
mprintf (z,"f_res %d var: |%s| \n",f_res,b0);
}


	return (f_res);

}





/*
 * -----------------------------------------------------------------------------------
 *
 *	agregar_stv_2
 *
 *
 * -----------------------------------------------------------------------------------
 */



int	agregar_stv_2(nf_alloc)
int	nf_alloc;
{
	char	b0[MSTR];
	char	b1[MSTR];
	char	b5[MSTR];
	char	b2[MAXV];

	char	cp_str[16];

	int	f_res;
	int	f_sigo;
	int	f_open_par;
	int	f_opt;

	int	caso;
	int	p1,p2,p3;
	int	i,j,k;
	int	l1,l2,l3,l4;
	int	c1,c2,c3;

	int	f1;

	int	st1,st2,st3,st4;

	char	z[MAXV];
	int	w;
	sprintf (z,"agregar_stv_2");
	w = g_dbg(z);

	/* copio string para trabajar local */
	strcpy(b0,pasar_a_minusc( (*fnp[nf_alloc]).l ));
	strcpy(b1, (*fnp[nf_alloc]).l );

	l2 = strlen(b0);


	strcpy(cp_str,",stat=stv_er");
	l3 = strlen(cp_str);

	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	/* largo definitivo de la sentencia */
	l2 = strlen(b0);
	

	f_res = 0;
	p1    = 0;
	p2    = l2 -1;

	st1 = 1;
	st2 = 0;
	st3 = 0;
	st4 = 1;

		
#if 0
	/* la primer palabra tiene que ser allocate */
	st1 = 0;
	for (i=0, f_sigo=1 ; f_sigo && i<=p2; i++)
	{	if (b0[i] == ' ' || b0[i] == '\t')
			p1++;
		else
		{	
			if (!strncmp(b0+i,"allocate",8))
			{
				f_sigo = 0;
				st1 = 1;
				p1 = i+8;
			}
		}
	}
#endif
	

	/* el ultimo caracter salvo blancos, es un parentesis que cierra */
	st2 = 0;
	l2 = strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i-- )
		if (b0[i] == ' ')
			p2--;
		else
		{
			if (b0[i] == ')')
			{
				f_sigo = 0;
				st2 = 1;
				p2  = i;
			}
			else
			{
				f_sigo = 0;
			}
		}

if (gp_debug && w)
{
mprintf (z,"paso 3 - el ultimo caracter salvo blanco es )           \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	/* caracteres especificos de allocate entre ( y ) */
	st3 = 1;
	for (i=p1; i<=p2; i++)
	{
		if (!((b0[i] >= 'a' && b0[i] <= 'z') || 
                      (b0[i] >= '0' && b0[i] <= '9') || 
		      b0[i] == '=' ||						/* por lineas con el bendito stat= */
                      b0[i] == ' ' || b0[i] == ':' || b0[i] == ',' || 
                      b0[i] == '-' || b0[i] == '(' || b0[i] == ')' || 
	              b0[i] == '_' || b0[i] == '*' || b0[i] == '+'    )  )
		{
			st3 = 0;
		}
	}

	
if (gp_debug && w)
{
mprintf (z,"paso 4 - caracteres especificos entre ( y )          \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}

#if 0
	f_sigo = 1;
	c2  = 0;
	c3  = 0;
	st4 = 0;
	i   = p1;

	while (f_sigo)
	{
		f_opt = 1;

		if (f_opt && c2 == 0 && b0[i] == '(' )
		{
			c2 = 1;
			f_opt = 0;
		}

		if (f_opt && c2 >= 1 && b0[i] == '(' )
		{
			c2++;
			f_opt = 0;
		}

		if (f_opt && c2 > 1 && b0[i] == ')' )
		{
			c2--;
			f_opt = 0;
		}

		if (f_opt && c2 == 1 && b0[i] == ',')
		{
			c3++;
			f_opt = 0;
		}
 
		i++;

		if (i> p2)
		{
			f_sigo = 0;
		}

	}

	if (c3)
	{
		st4 = 1;
		
		if (tiene_stat(b0) && c3 == 1 )
			st4 = 0;
	}

#endif

if (gp_debug && w)
{
mprintf (z,"paso 5 - hay comas sin estar entre    ( y )  \n");
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"b1          |%s| \n",b1);
mprintf (z,"st1 st2 st3 |%d,%d,%d,%d| \n",st1,st2,st3,st4);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	if (st1 && st2 && st3 && st4)
		f_res = 1;

	/*
	 * ver despues como manejar los diferentes casos !!
	 * caso 1 - allocate en una linea, con una sola variable 
	 *
	 * STAT=STV_ER
	 */
 	
	caso = 1;
	
	if (f_res = 1 && caso == 1)
	{

if (gp_debug && w)
{
mprintf (z,"p1: %d %c p2: %d %c\n",p1,b0[p1],p2,b0[p2]);
mprintf (z,"b0: |%s| \n",b0);
mprintf (z,"b1: |%s| \n",b1);
}

		
		l4 = strlen(b0);
		memset(b2,0,sizeof(b2));
		memset(b5,0,sizeof(b5));


		strcpy(b5,b1);
		b5[p2]=0;
		strcat(b5,cp_str);
		strcat(b5,b1+p2);

if (gp_debug && w)
{
mprintf (z,"p1 p2 p3, %2d %2d %2d i %2d  \n",p1,p2,p3,i);
mprintf (z,"b0: |%s| \n",b0);
mprintf (z,"b1: |%s| \n",b1);
mprintf (z,"b5: |%s| \n",b5);
}

		strcpy( (*fnp[nf_alloc]).l, b5);

#if 0
		/* en p1,p2 estan los ( ) */
		for (i=p1+1, p3=0, f_sigo=1, f_res = 0; f_sigo && i<= p2-1; i++)
		{
			if ( (b0[i] >= 'a' && b0[i] <= 'z') || (b0[i] >= '0' && b0[i] <= '9') ||
                              b0[i] == '_' )
			{
				b2[p3] = b1[i];
				p3++;
			}
			else
				f_sigo=0, f_res=1;
		}
#endif
	}
	else
		error(9005);		
	

if (gp_debug && w)
{
mprintf (z,"paso 5           \n");
mprintf (z,"f_res %d var: b0: |%s| \n",f_res,b0);
}


	return (f_res);

}




/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_check_alloc_ok
 *
 *
 * -----------------------------------------------------------------------------------
 */



int	tiene_check_alloc_ok(nf_alloc,nf_check,nkey)
int	nf_alloc;
int	*nf_check;
int	*nkey;
{
	char	m0[MSTR];
	char	m1[MSTR];
	char	b1[MAXB];
	int	i,j,k;
	int	l1,l2,l3;
	int	f1,f2;

	int	f_res;
	int	f_sigo;
	int	linea_check;

	char	z[MAXV];
	int	w;
	sprintf (z,"tiene_alloc_ok");
	w = g_dbg(z);

	memset(b1,0,sizeof(b1));
	f_res = 0;
	f_sigo = 1;
	k = 0;


	do
	{
		strcpy(m0,pasar_a_minusc ( (*fnp[nf_alloc + k+0]).l ));
		strcpy(m1,pasar_a_minusc ( (*fnp[nf_alloc + k+1]).l ));
		l2 = strlen(m0);
		l3 = strlen(m1);
		
		/* el check alloc esta ok, si estoy utilizando la variable stv_er ... */
		if (tiene_string(m0,"check_alloc") && 
		   (tiene_string(m0,"stv_er") || tiene_string(m1,"stv_er")  )   )
		{
			f_sigo = 0;
			f_res = 1;
			
			/* busco el key utilizado */
			for (j=0, f1=1; f1 && j<l2; j++)
			{
				if (m0[j] == ':')
				{
					strncpy(b1,m0+j+1,4);
					*nkey = atoi(b1);
					*nf_check = nf_alloc + k;
					f1=0;
				}
			}
		}
		
		/* el check alloct tiene que estar cerca del allocate!! */
		k++;
		if (k > 3)
			f_sigo = 0;

	} 
	while (f_sigo);

#if 0
	/* tendria que tener la var stv_er ?? */
	if (f_res)
	{


	}
#endif

		
	return (f_res);
}











/*
 * -----------------------------------------------------------------------------------
 *
 *	arma_dos_lineas_con_check 
 *
 * -----------------------------------------------------------------------------------
 */


int	arma_dos_lineas_con_check(s,b1,b2)
char	*s;
char	*b1;
char	*b2;
{
	char	blanks[MAXB];
	int	i,j,k;
	int	p1,p2;
	int	f1;
	int	l2;


	memset(blanks,32,sizeof(blanks));
	l2=strlen(s);

	for (i=0, f1=1, p1=0; f1 && i<l2; i++)
		if (s[i] == ',')
			f1=0, p1=i;

	if (p1 == 0)
		error (9019);

	strcpy(b1,s);
	b1[p1+1] = 0;
	strcat(b1," &");

	strncpy(b2,blanks,74-strlen(s+p1+1));
	strcat(b2,s+p1+1);

	return (0);
}


int	arma_dos_lineas_con_check_for(s,b1,b2)
char	*s;
char	*b1;
char	*b2;
{
	char	blanks[MAXB];
	int	i,j,k;
	int	p1,p2;
	int	f1;
	int	l2;


	memset(blanks,32,sizeof(blanks));
	l2=strlen(s);

	for (i=0, f1=1, p1=0; f1 && i<l2; i++)
		if (s[i] == ',')
			f1=0, p1=i;

	if (p1 == 0)
		error (9019);

	strcpy(b1,s);
	b1[p1+1] = 0;

	strncpy(b2,blanks,70-strlen(s+p1+1));
	strcat(b2,s+p1+1);
	b2[5] = '+';

	return (0);
}


int	arma_dos_lineas_con_alloc_for_v1(s,b1,b2)
char	*s;
char	*b1;
char	*b2;
{
	char	blanks[MAXB];
	int	i,j,k;
	int	p1,p2;
	int	f1;
	int	l2;


	memset(blanks,32,sizeof(blanks));
	l2=strlen(s);

	for (i=0, f1=1, p1=0; f1 && i<l2; i++)
		if (!strncmp(s+i,",stat=stv_er)",13))
			f1=0, p1=i;

	if (p1 == 0)
		error (9019);

	strcpy(b1,s);
	b1[p1+1] = 0;

	strncpy(b2,blanks,70-strlen(s+p1+1));
	strcat(b2,s+p1+1);
	b2[5] = '+';

	return (0);
}


int	arma_dos_lineas_con_alloc_for_v2(s,b1,b2)
char	*s;
char	*b1;
char	*b2;
{
	char	blanks[MAXB];
	int	i,j,k;
	int	p1,p2;
	int	f1;
	int	l2;


	memset(blanks,32,sizeof(blanks));
	l2=strlen(s);

	for (i=0, f1=1, p1=0; f1 && i<l2; i++)
		if (!strncmp(s+i,",",1))
			f1=0, p1=i;

	if (p1 == 0)
		error (9019);

	strcpy(b1,s);
	b1[p1+1] = 0;

	strncpy(b2,blanks,70-strlen(s+p1+1));
	strcat(b2,s+p1+1);
	b2[5] = '+';

	return (0);
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	grabar_mapa
 *	graba lineas de log de cambios realizados en sentencias alloc
 *
 * -----------------------------------------------------------------------------------
 */


int	grabar_mapa(n_men,tipo,s1,n1,pf,uf)
int	n_men;
int	tipo;
char	*s1;
int	n1,pf,uf;
{
	char	m0[MSTR];


	if (n_men == 0)
	{
		fprintf (hfou5,"------------------------------------------(%2d)-\n",uf);
	}

	if (n_men == 1)
	{
		fprintf (hfou5,"Type (%2d)          src:                       |%s|\n",tipo,s1);
	}

	if (n_men == 6)
	{
		fprintf (hfou5,"Allocate number       :   %3d                 | \n",n1);
	}

	if (n_men == 2)
	{	
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"alloc sentence in line: %5d (%5d - %5d) |%s|\n",n1,pf,uf,m0);
	}

	if (n_men == 3)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"block starts at line  : %5d                 |%s|\n",pf,m0);
	}
	
	if (n_men == 4)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"block ends   at line  : %5d                 |%s|\n",uf,m0);
	}

	if (n_men == 5)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"line to insert use all: %5d (%5d - %5d) |%s|\n",n1,pf,uf,m0);
	}

	if (n_men == 7)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"line to insert check  : %5d (%5d - %5d) |%s|\n",n1,pf,uf,m0);
	}
		
	if (n_men == 8)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"chk alloc fnd in line : %5d (key %04d)      |%s|\n",n1,pf,m0);
	}

	if (n_men == 10)
	{
		fprintf (hfou5,"key used              :   %3d                 | \n",n1);
	}


	if (n_men == 11)
	{
		fprintf (hfou5,"line to replace allocs: %5d  (%3d allocs)   |%s| \n",n1,pf,s1);
	}

	if (n_men == 12)
	{
		fprintf (hfou5,"allocate added        : %5d                 |%s|\n",tipo,s1);
	}

}



/*
 * -----------------------------------------------------------------------------------
 *
 *	grabar_plan 
 *
 * -----------------------------------------------------------------------------------
 */


int	grabar_plan(n_men,tl,tv,st,ok,prog_name,m0)
int	n_men,tl,tv,st,ok;
char	*prog_name,*m0;
{
	char	stl[3][4] = { "  ","sl","ml" };
	char	stv[3][4] = { "  ","sv","mv" };
	char	sst[3][4] = { "  ","st","  " };
	char	sok[3][4] = { "  ","ok","no" };

#if 0
	if (n_men == 1 || n_men == 9)
#endif

	if (1)
	{
		fprintf (hfou4,"(%02d) %s %s %s %s %-30.30s |%s|\n",n_men,stl[tl],stv[tv],sst[st],sok[ok],prog_name,m0 );

	}

#if 0
								grabar_plan(1,1,1,0,0,prog_name,m1);
								grabar_plan(9,1,1,0,0,prog_name,m1);
									grabar_plan(10,1,1,1,0,prog_name,m1);  2 !!! si es no ok
								grabar_plan(9,2,2,0,0,prog_name,m1);
#endif



}




#if 1
/*
 * -----------------------------------------------------------------------------------
 *
 *	grabar_mapa
 *	graba lineas de log de cambios realizados en sentencias alloc
 *
 * -----------------------------------------------------------------------------------
 */


int	grabar_mapa_rf(n_men,tipo,s1,n1,pf,uf)
int	n_men;
int	tipo;
char	*s1;
int	n1,pf,uf;
{
	char	m0[MSTR];


#if 0
	if (n_men == 0)
	{
		fprintf (hfou5,"------------------------------------------(%2d)-\n",uf);
	}

	if (n_men == 1)
	{
		fprintf (hfou5,"Type (%2d)          src:                       |%s|\n",tipo,s1);
	}

	if (n_men == 6)
	{
		fprintf (hfou5,"Allocate number       :   %3d                 | \n",n1);
	}

	if (n_men == 2)
	{	
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"alloc sentence in line: %5d (%5d - %5d) |%s|\n",n1,pf,uf,m0);
	}

	if (n_men == 3)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"block starts at line  : %5d                 |%s|\n",pf,m0);
	}
	
	if (n_men == 4)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"block ends   at line  : %5d                 |%s|\n",uf,m0);
	}

	if (n_men == 5)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"line to insert use all: %5d (%5d - %5d) |%s|\n",n1,pf,uf,m0);
	}

	if (n_men == 7)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"line to insert check  : %5d (%5d - %5d) |%s|\n",n1,pf,uf,m0);
	}
		
	if (n_men == 8)
	{
		strcpy(m0,trim_blanks_beg(s1));
		fprintf (hfou5,"chk alloc fnd in line : %5d (key %04d)      |%s|\n",n1,pf,m0);
	}

	if (n_men == 10)
	{
		fprintf (hfou5,"key used              :   %3d                 | \n",n1);
	}


	if (n_men == 11)
	{
		fprintf (hfou5,"line to replace allocs: %5d  (%3d allocs)   |%s| \n",n1,pf,s1);
	}

	if (n_men == 12)
	{
		fprintf (hfou5,"allocate added        : %5d                 |%s|\n",tipo,s1);
	}

#endif

}



/*
 * -----------------------------------------------------------------------------------
 *
 *	grabar_plan 
 *
 * -----------------------------------------------------------------------------------
 */


int	grabar_plan_rf(n_men,tl,tv,st,ok,prog_name,m0)
int	n_men,tl,tv,st,ok;
char	*prog_name,*m0;
{
	char	stl[3][4] = { "  ","sl","ml" };
	char	stv[3][4] = { "  ","sv","mv" };
	char	sst[3][4] = { "  ","st","  " };
	char	sok[3][4] = { "  ","ok","no" };

#if 0
	if (n_men == 1 || n_men == 9)
#endif

	if (1)
	{
		fprintf (hfou7,"(%02d) %s %s %s %s %-30.30s |%s|\n",n_men,stl[tl],stv[tv],sst[st],sok[ok],prog_name,m0 );

	}

#if 0
								grabar_plan(1,1,1,0,0,prog_name,m1);
								grabar_plan(9,1,1,0,0,prog_name,m1);
									grabar_plan(10,1,1,1,0,prog_name,m1);  2 !!! si es no ok
								grabar_plan(9,2,2,0,0,prog_name,m1);
#endif



}

#endif












/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_t09
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_t09
 *	agregar check alloc de sentencias con allocate 
 *	tipo 09 sl sv
 *
 *	recibe numero de file donde encontro allocate y la linea del allocate
 *	num_f		numero de file en tabla tb
 *	nf_alloc	numero de fila donde esta el alloc
 *	add_lines	cuantas lineas agrego al file
 *	num_alloc	el numero de alloc real secuencual en archivo
 *	f_act		0 no actualiza numeros de lineas, 1 si 
 *
 *	Atenti:
 *	esto corrije un solo allocate x vez !!!
 */

int	chg_alloc_t09(num_f,nf_alloc,add_lines,num_alloc,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	f_act;
{
	static	int	f_miscmod1 = 0;
	static	int	f_miscmod2 = 0;

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	f_tengo;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;
	int	vl[5],kvl,vl_min,vl_max;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	pri_s;		/* primer save */
	int	pri_i;		/* implicit none */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_use;	/* linea en la que hay que poner el use */
	int	agrego_lines;	/* lineas que se agregan al final x hacer-lugar */

	char	m0[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_t09");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;

	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;


	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	
		grabar_mapa(6,0," ",num_alloc,0,0);
		grabar_mapa(2,0,b1,lne(nf_alloc),pf,uf);

if (gp_debug && w)
{
mprintf (z,"entro a busco_pri_l pf: %d uf: %d nf_alloc: %d \n",pf,uf,nf_alloc);
}
		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9001);

if (gp_debug && w)
{
mprintf (z,"sali de busco_pri_l pf: %d uf: %d nf_alloc: %d \n",pf,uf,nf_alloc);
mprintf (z,"sali de busco_pri_l pri_l: %d mod_type: %d \n",pri_l,mod_type);
mprintf (z,"sali de busco_pri_l pri_l: %d |%s| \n",pri_l,(*fnp[pri_l]).l);
}

		grabar_mapa(3,0,(*fnp[pri_l]).l,0,lne(pri_l),0);

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

if (gp_debug && w)
{
mprintf (z,"sali de busco_ult_l pf: %d uf: %d nf_alloc: %d \n",pf,uf,nf_alloc);
mprintf (z,"sali de busco_ult_l ult_l: %d mod_type: %d \n",ult_l,mod_type);
mprintf (z,"sali de busco_ult_l ult_l: %d |%s| \n",ult_l,(*fnp[ult_l]).l);
}


		grabar_mapa(4,0,(*fnp[ult_l]).l,0,0,lne(ult_l));

		/* busco donde colocar el use allocate_vars */
		f_tengo = 0;

		/* busco el ultim use ... */
		ult_u = 0;
		busco_ult_u(pri_l,ult_l,pri_l,&ult_u);

		/* busco primer declaracion de variables ... */
		pri_d = 0;
		busco_pri_d(pri_l,ult_l,pri_l,&pri_d);

		/* busco primer save */
		pri_s = 0;
		busco_pri_s(pri_l,ult_l,pri_l,&pri_s);

#if 1
		/* busco si esta implicit none */
		pri_i = 0;
		busco_pri_i(pri_l,ult_l,pri_l,&pri_i);
#endif


		/* solo para los que salieron distinto de 0 .. busco el minimo valor de linea */
		kvl=0;
		if (ult_u)
			vl[kvl++] = ult_u+1;
		if (pri_d)
			vl[kvl++] = pri_d;
		if (pri_s)
			vl[kvl++] = pri_s;
		if (pri_i)
			vl[kvl++] = pri_i;


if (gp_debug && w)
{
	mprintf (z,"lineas posibles para use:  ult_u %d pri_d %d pri_s %d \n",ult_u,pri_d,pri_s);
	mprintf (z,"|%s|\n",(*fnp[ult_u]).l );
	mprintf (z,"|%s|\n",(*fnp[pri_d]).l );
	mprintf (z,"|%s|\n",(*fnp[pri_s]).l );
}

		/* no encontre donde poner el allocate_vars ! */
		if (kvl == 0)
		{
			/* hay modulos que no tienen ni ult_u, ni pri_d, ni pri_s  !!! */
			if (!strncmp(prog_name,"capacity_arrays.f90",19) || !strncmp(prog_name,"p1.f90",6))
			{	vl[0]=pri_l+1;
				kvl=1;
			}
			else
				error(9003);
		}

		vl_min = 100000;
		for (k=0; k<kvl; k++)
			if (vl[k] < vl_min)
				vl_min = vl[k];

		/* use allocate_vars va en la primer linea que cumple condicion */
		linea_use = vl_min;
		grabar_mapa(5,0,(*fnp[linea_use]).l,lne(linea_use),lne(pri_l),lne(ult_l));

if (gp_debug && w)
{
	mprintf (z,"detecte linea para poner el use_allocate en ... %d \n",linea_use);
	mprintf (z,"|%s|\n",(*fnp[linea_use+0]).l );
	mprintf (z,"|%s|\n",(*fnp[linea_use+1]).l );
}


		/* 
		 * a partir de aqui ... a hacer los cambios !! 
		 * correr todo 1 linea para abajo, y agregar el use allocate_vars.f90 
		 */


		/*
		 * La linea para poner use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 */
 
		agrego_lines = 3;
		*add_lines = 0;

if (gp_debug && w)
{
	mprintf (z,"voy if(!busco_use ...  pri_l %d ult_l %d \n",pri_l,ult_l);
	mprintf (z,"|%|\n",(*fnp[pri_l]).l);
	mprintf (z,"|%|\n",(*fnp[ult_l]).l);
}

		if (!busco_use_allocate(pri_l,ult_l))
		{
if (gp_debug && w)
{
	mprintf (z,"entre al if (!busco_use  ... no encontre allocate !! \n");
}

			hacer_lugar_itz(pf,qf_src-1,linea_use,agrego_lines,f_act);

			c5 = n_blanks_beg( (*fnp[pri_l]).l );
			memset(blanks,0,sizeof(blanks));
			memset(blanks,32,c5);

			strcpy( (*fnp[linea_use]).l  ,blanks);
			strcat( (*fnp[linea_use]).l  ,"use allocate_vars");

			if (!busco_use_miscmod(pri_l,ult_l))
			{
				strcpy( (*fnp[linea_use+1]).l  ,blanks);
				strcat( (*fnp[linea_use+1]).l  ,"use miscmod");
			}
			else
			{
				strcpy( (*fnp[linea_use+1]).l  ,"     ");
			}

			strcpy( (*fnp[linea_use+2]).l,"     ");
			*add_lines = agrego_lines;

		}

		flag_alloc_ok = 1;
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_t13
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_t13
 *	agregar check alloc de sentencias con allocate tipo 13 
 *	recibe numero de file donde encontro allocate y la linea del allocate
 *
 *	Atenti:
 *	esto corrije un solo allocate x vez !!!
 *
 */

int	chg_alloc_t13(num_f,nf_alloc,add_lines,num_alloc,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	f_act;
{
	static	int	f_miscmod1 = 0;
	static	int	f_miscmod2 = 0;

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;

	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_use;	/* linea en la que hay que poner el use */
	int	agrego_lines;	/* lineas que se agregan al final x hacer-lugar */

	char	m0[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_t13");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;


	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;

	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	
		grabar_mapa(6,0," ",num_alloc,0,0);
		grabar_mapa(2,0,b1,lne(nf_alloc),pf,uf);

if (gp_debug && w)
{
mprintf (z,"entro a busco_pri_l pf: %d uf: %d nf_alloc: %d \n",pf,uf,nf_alloc);
}
		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9016);

if (gp_debug && w)
{
mprintf (z,"sali de busco_pri_l pf: %d uf: %d nf_alloc: %d \n",pf,uf,nf_alloc);
mprintf (z,"sali de busco_pri_l pri_l: %d mod_type: %d \n",pri_l,mod_type);
mprintf (z,"sali de busco_pri_l pri_l: %d |%s| \n",pri_l,(*fnp[pri_l]).l);
}

		grabar_mapa(3,0,(*fnp[pri_l]).l,0,lne(pri_l),0);

		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

		grabar_mapa(4,0,(*fnp[ult_l]).l,0,0,lne(ult_l));


		/* busco donde colocar el use ... allocate_vars */
		ult_u = 0;
		busco_ult_u(pri_l,ult_l,pri_l,&ult_u);


		/* busco primer declaracion de variables */
		pri_d = 0;
		busco_pri_d(pri_l,ult_l,pri_l,&pri_d);

		/* TO_DO  aqui habria que utilizar el mismo metodo que en t09 */

		f_try = 0;

		if (!f_try && ult_u)
		{
			linea_use = ult_u+1;
			f_try = 1;
		}

		if (!f_try && pri_d)
		{
			linea_use = pri_d;
			grabar_mapa(5,0,(*fnp[linea_use]).l,lne(linea_use),lne(pri_l),lne(ult_l));
			f_try = 1;
		}

		/* si no encontro donde colocar el use .. usamos la pri_l */
		if (!f_try)
		{	
			linea_use = pri_l + 1;
#if 0
			for (h=pf; h<=uf; h++)
				fprintf (hfou6,"%6d,|%s|\n",h,(*fnp[h]).l);
			printf ("allocate: |%s| \n",(*fnp[nf_alloc]).l );
			printf ("pri_l  %d |%s| \n",pri_l,(*fnp[pri_l]).l );
			printf ("ult_l  %d |%s| \n",ult_l,(*fnp[ult_l]).l );
			error(9014);
#endif
		}


		/* a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo 1 linea para abajo, y agregar el use allocate_vars.f90 
		 * 2) en la linea del allocate, agregar  stat= ... var de error 
		 * 3) correr todo 1 linea para abajo, y agregar call check_alloc ...
		 *    con variables :
		 *    identificador (string ... nombre de programa con un numero de allocate )
		 *    nombre de la variable ...	
		 *    variable de error 
		 *    opcional ... extra_info ?? 
		 */


		/*
		 * La linea para poner use allocate_vars es linea_use
		 * correr todo para abajo una linea desde ahi ...
		 */
 
		agrego_lines = 3;
		*add_lines = 0;


		if (!busco_use_allocate(pri_l,ult_l))
		{
			hacer_lugar_itz(pf,qf_src-1,linea_use,agrego_lines,f_act);

			c5 = n_blanks_beg( (*fnp[pri_l]).l );
			memset(blanks,0,sizeof(blanks));
			memset(blanks,32,c5);

			strcpy( (*fnp[linea_use]).l  ,blanks);
			strcat( (*fnp[linea_use]).l  ,"use allocate_vars");

			if (!busco_use_miscmod(pri_l,ult_l))
			{
				strcpy( (*fnp[linea_use+1]).l  ,blanks);
				strcat( (*fnp[linea_use+1]).l  ,"use miscmod");
			}
			else
			{
				strcpy( (*fnp[linea_use+1]).l  ,"     ");
			}

			strcpy( (*fnp[linea_use+2]).l,"     ");
			*add_lines = agrego_lines;

		}

		flag_alloc_ok = 1;
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	chg_alloc_t15
 *
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	chg_alloc_t15
 *	agregar check alloc de sentencias con allocate tipo 15 
 *	el mas complejo ... multiple linea, varias variables ...
 *	recibe numero de file donde encontro allocate y la linea del allocate
 *	hay aprox 150 de esos en el code-base (141 a la fecha de hoy)
 *	Atenti:
 *	esto corrije un solo allocate x vez !!!
 *
 */

int	chg_alloc_t15(num_f,nf_alloc,add_lines,num_alloc,f_act)
int	num_f;
int	nf_alloc;
int	*add_lines;
int	num_alloc;
int	f_act;
{

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4,c5;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	f_hay_alloc;
	int	f_try;
	int	vl[5],kvl,vl_min,vl_max;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	char	blanks[64];
	int	pf,uf,nf,qf;
	int	tipo_ext;
	int	pri_l,ult_l;
	int	ult_u,pri_d;	/* ultimo use, primera declaracion */
	int	pri_s;		/* primer save */
	int	pri_i;		/* implicit none */
	int	mod_type;	/* 0 no se, 1 subroutine 2 function */
	int	linea_use;	/* linea en la que hay que poner el use */
	int	agrego_lines;	/* lineas que se agregan al final x hacer-lugar */

	char	m0[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"chg_alloc_t15");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	nf = num_f;

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;

	/* primera y ultima linea del fuente */
	pf = (*tb[nf]).pf;
	uf = (*tb[nf]).uf;
	qf = uf - pf +1;


	/* nombre y ext del file  */
	strcpy(prog_name,f_name(pf));
	strcpy(exte_name,e_name(pf));

	if (!strncmp(exte_name,"for",3))
		tipo_ext = 1;

	if (!strncmp(exte_name,"f90",3))
		tipo_ext = 2;


	/* proceso linea i */
	strcpy(b0,(*fnp[nf_alloc]).l );
	strcpy(b1, pasar_a_minusc(b0));

	l2 = strlen(b1);

	f_proceso = 1;
	if (linea_vacia(b1) || es_linea_comentario(b1))
		f_proceso = 0;


	/* solo proceso lineas que no son comentario ni vacias */
	if (f_proceso)
	{	

		grabar_mapa(6,0," ",num_alloc,0,0);
		grabar_mapa(2,0,b1,lne(nf_alloc),pf,uf);

#if 1

		if (!busco_pri_l(pf,uf,nf_alloc,&pri_l,&mod_type))
			error(9007);

		grabar_mapa(3,0,(*fnp[pri_l]).l,0,lne(pri_l),0);


		if (!busco_ult_l(pf,uf,nf_alloc,&ult_l,mod_type))
			error(9002);

		grabar_mapa(4,0,(*fnp[ult_l]).l,0,0,lne(ult_l));


		/* busco el ultim use ... */
		ult_u = 0;
		busco_ult_u(pri_l,ult_l,pri_l,&ult_u);

		/* busco primer declaracion de variables ... */
		pri_d = 0;
		busco_pri_d(pri_l,ult_l,pri_l,&pri_d);

		/* busco primer save */
		pri_s = 0;
		busco_pri_s(pri_l,ult_l,pri_l,&pri_s);

#if 1
		/* busco si esta implicit none */
		pri_i = 0;
		busco_pri_i(pri_l,ult_l,pri_l,&pri_i);
#endif

		/* solo para los que salieron distinto de 0 .. busco el minimo valor de linea */
		kvl=0;
		if (ult_u)
			vl[kvl++] = ult_u+1;
		if (pri_d)
			vl[kvl++] = pri_d;
		if (pri_s)
			vl[kvl++] = pri_s;
		if (pri_i)
			vl[kvl++] = pri_i;

		/* no encontre donde poner el allocate_vars ! */
		if (kvl == 0)
			error(9021);

		vl_min = 100000;
		for (k=0; k<kvl; k++)
			if (vl[k] < vl_min)
				vl_min = vl[k];
			
		/* use allocate_vars va en la primer linea que cumple condicion */
		linea_use = vl_min;
		grabar_mapa(5,0,(*fnp[linea_use]).l,lne(linea_use),lne(pri_l),lne(ult_l));


#endif



#if 0
		/* TO_DO  aqui habria que utilizar el mismo metodo que en t09 */

		f_try = 0;

		if (!f_try && ult_u)
		{
			linea_use = ult_u+1;
			f_try = 1;
		}

		if (!f_try && pri_d)
		{
			linea_use = pri_d;
			grabar_mapa(5,0,(*fnp[linea_use]).l,lne(linea_use),lne(pri_l),lne(ult_l));
			f_try = 1;
		}

		/* que pasa si no hay ni use ni dec de variables ?? */
		if (!f_try)
		{	
			for (h=pf; h<=uf; h++)
				fprintf (hfou6,"%6d,|%s|\n",h,(*fnp[h]).l);
			printf ("allocate: |%s| \n",(*fnp[nf_alloc]).l );
			printf ("pri_l  %d |%s| \n",pri_l,(*fnp[pri_l]).l );
			printf ("ult_l  %d |%s| \n",ult_l,(*fnp[ult_l]).l );
			error(9015);
		}


#endif

		/* a partir de aqui ... a hacer los cambios !! 
		 *
		 * 1) correr todo 3 linea para abajo
		 * 2) chequear si esta el allocate_vars en el bloque
		 * 3) chequear si esta miscmod 
		 */


		/*
		 * La linea para poner use allocate_vars es linea_use
		 * correr todo para abajo tres lineas desde ahi ...
		 *
		 */
 
		agrego_lines = 3;
		*add_lines = 0;


		if (!busco_use_allocate(pri_l,ult_l))
		{
if (gp_debug && w)
{
mprintf (z,"pf %d qf_src-1 %d linea_use %d agrego_lines %d \n",z,pf,qf_src-1,linea_use,agrego_lines);
mprintf (z,"entro a hacer lugar \n",z);
}

			hacer_lugar_itz(pf,qf_src-1,linea_use,agrego_lines,f_act);
if (gp_debug && w)
{
mprintf (z,"volvi de hacer lugar \n",z);
mprintf (z,"pf %d qf_src-1 %d linea_use %d agrego_lines %d \n",z,pf,qf_src-1,linea_use,agrego_lines);
mprintf (z,"voy a c5 = b_blanks_beg \n",z);
}

			c5 = n_blanks_beg( (*fnp[pri_l]).l );
			memset(blanks,0,sizeof(blanks));
			memset(blanks,32,c5);

			strcpy( (*fnp[linea_use]).l  ,blanks);
			strcat( (*fnp[linea_use]).l  ,"use allocate_vars");


			if (!busco_use_miscmod(pri_l,ult_l))
			{
				strcpy( (*fnp[linea_use+1]).l  ,blanks);
				strcat( (*fnp[linea_use+1]).l  ,"use miscmod");
			}
			else
			{
				strcpy( (*fnp[linea_use+1]).l  ,"     ");
			}

			strcpy( (*fnp[linea_use+2]).l,"     ");
			*add_lines = agrego_lines;
		}

		flag_alloc_ok = 1;
	}
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	hacer_lugar
 *	mueve lineas de vector de lineas fnp para hacer lugar
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	correr todas las lineas en el vector fnp n lugares
 *	hay que agregar lineas al final 
 *
 *	pf		primera fila del bloque que condiero
 *	uf		ultima fila del bloque
 *	linea		linea desde la que corro
 *	qlin		cant de lineas que corro
 *	f_act		0 no actualizo tabla tb 1 si
 */

int	hacer_lugar(pf,uf,linea,qlin,f_act)
int	pf;
int	uf;
int	linea;
int	qlin;
int	f_act;
{
	int	i,j,k,h;
	int	new_uf;
	int	add_lines;
	int	f1,n_f;
	int	d0[MAXV];

	char	z[MAXV];
	int	w;
	sprintf (z,"hacer_lugar");
	w = g_dbg(z);

	add_lines = qlin;
	new_uf    = uf + qlin;


	/* big OJO !!
	 * pf y uf ... son parametros !!!!
	 */

	i = linea;
	/* me fijo lineas */
	for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
		if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
		{	n_f = j;
			pf = (*tb[j]).pf;
			uf = (*tb[j]).uf;
			f1=0;
		}


if (gp_debug && w)
{
mprintf (z,"pf %d uf %d linea %d q_lin %d new_uf %d \n",pf,uf,linea,qlin,new_uf);
}

	for (i=1; i<=qlin; i++)
	{
		k = uf+i;

if (gp_debug && w)
{
mprintf (z,"agrego linea al final k: %d   \n",k);
}


		/* agrego lineas al final */
		fnp[k] = (fnptr  ) malloc (sizeof (node));
		if ( fnp[k] == NULL)
			error(917);

		(*fnp[k]).l[0] = 0;
		(*fnp[k]).f1 = 0;
		(*fnp[k]).f2 = 0;
		(*fnp[k]).f3 = 0;
	}

if (gp_debug && w)
{
mprintf (z,"corro lin desde %d a %d tot %d lin   \n",new_uf,linea+qlin,new_uf - (linea+qlin)  +1  );

}

	for (j=new_uf; j >= linea+qlin; j-- )
	{
if (gp_debug && w)
{
	mprintf (z,"copio de  %d |%s a %d |%s| \n",j,fnp[j-qlin],j-qlin,fnp[j]);
}
		memcpy ( fnp[j],fnp[j-qlin], sizeof (node) );
	}

if (gp_debug && w)
{
mprintf (z,"vacio lin desde %d a %d tot %d lin   \n",linea,linea+qlin-1, linea+qlin-1 -linea + 1  );

}
#if 1
	for (i=linea ; i< linea+qlin; i++)
	{

if (gp_debug && w)
{
	mprintf (z,"Voy a limpiar %d |%s| \n",i,(*fnp[i]).l);
}

		(*fnp[i]).l[0] = 0;
		(*fnp[i]).f1 = 0;
		(*fnp[i]).f2 = 0;
		(*fnp[i]).f3 = 0;

#if 1
		sprintf (d0,"agregada x hacer lugar i: %d",i);
#endif
#if 0
		sprintf (d0,"    ");
#endif

		strcpy( (*fnp[i]).l,d0);
	}
#endif

	/* tengo que actualizar situacion de numeros en tablas */
	if (f_act)
	{
		for (h=0; h<qf_ff; h++)
		{	if (h)
			(*tb[h]).pf += add_lines;
			(*tb[h]).uf += add_lines;
		}

		uf = (*tb[n_f]).uf;
		qf_src += add_lines;
	}


if (gp_debug && w)
{
mprintf (z,"listado de lineas %d a %d   \n",pf,new_uf );
#if 0
for (i=pf; i<= new_uf; i++)
	mprintf (z,"(%3d)|%s|\n",i,(*fnp[i]).l);
#endif
mprintf (z,"salgo   \n" );
mprintf (z,"-----   \n\n" );
}

}


#if 1
int	hacer_lugar_itz(pf,uf,linea,qlin,f_act)
int	pf,uf,linea,qlin,f_act;
{
	hacer_lugar_mf(linea,qlin);
}

#endif

#if 0
int	hacer_lugar_itz(pf,uf,linea,qlin,f_act)
int	pf,uf,linea,qlin,f_act;
{
	hacer_lugar(pf,uf,linea,qlin,f_act);

}
#endif



/*
 * -----------------------------------------------------------------------------------
 *
 *	hacer_lugar_mf
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	Version de hacer_lugar para combinar con mover_bulk
 *	suponemos que seguimos trabajando con fnp
 *	y que la linea que indico, es el subindice de la 
 *	linea en el vector fnp
 *
 *	linea		linea desde la que corro
 *	qlin		cant de lineas que corro
 */

int	hacer_lugar_mf(linea,qlin)
int	linea;
int	qlin;
{
	int	i,j,k,h;
	int	pf,uf;
	int	new_uf;
	int	add_lines;
	int	f1,n_f;
	int	d0[MAXV];
	int	f_act = 1;

	char	z[MAXV];
	int	w;
	sprintf (z,"hacer_lugar_mf");
	w = g_dbg(z);

	/* que mover_bulk se arregle para mover los archivos */
	n_f = n_name(linea);
	mover_bulk(n_f+1,qlin);


#if 1


	i = linea;
	/* me fijo lineas */
	for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
		if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
		{	n_f = j;
			pf = (*tb[j]).pf;
			uf = (*tb[j]).uf;
			f1=0;
		}

	new_uf    = uf ;		/* mover bulk ya sumo qlin !! */

#if 0
	add_lines = qlin;
	new_uf    = uf + qlin;
#endif

if (gp_debug && w)
{
mprintf (z,"pf %d uf %d linea %d q_lin %d new_uf %d \n",pf,uf,linea,qlin,new_uf);
}

#if 0
	for (i=1; i<=qlin; i++)
	{
		k = uf+i;

if (gp_debug && w)
{
mprintf (z,"agrego linea al final k: %d   \n",k);
}


		/* agrego lineas al final */
		fnp[k] = (fnptr  ) malloc (sizeof (node));
		if ( fnp[k] == NULL)
			error(917);

		(*fnp[k]).l[0] = 0;
		(*fnp[k]).f1 = 0;
		(*fnp[k]).f2 = 0;
		(*fnp[k]).f3 = 0;
	}
#endif


if (gp_debug && w)
{
mprintf (z,"corro lin desde %d a %d tot %d lin   \n",new_uf,linea+qlin,new_uf - (linea+qlin)  +1  );

}

	for (j=new_uf; j >= linea+qlin; j-- )
	{
if (gp_debug && w)
{
	mprintf (z,"copio de  %d |%s a %d |%s| \n",j,fnp[j-qlin],j-qlin,fnp[j]);
}
		memcpy ( fnp[j],fnp[j-qlin], sizeof (node) );
	}

if (gp_debug && w)
{
mprintf (z,"vacio lin desde %d a %d tot %d lin   \n",linea,linea+qlin-1, linea+qlin-1 -linea + 1  );

}
#if 1
	for (i=linea ; i< linea+qlin; i++)
	{

if (gp_debug && w)
{
	mprintf (z,"Voy a limpiar %d |%s| \n",i,(*fnp[i]).l);
}

		(*fnp[i]).l[0] = 0;
		(*fnp[i]).f1 = 0;
		(*fnp[i]).f2 = 0;
		(*fnp[i]).f3 = 0;

#if 1
		sprintf (d0,"agregada x hacer lugar i: %d",i);
#endif
#if 0
		sprintf (d0,"    ");
#endif

		strcpy( (*fnp[i]).l,d0);
	}
#endif

#if 0
	/* tengo que actualizar situacion de numeros en tablas */
	if (f_act)
	{
		for (h=0; h<qf_ff; h++)
		{	if (h)
			(*tb[h]).pf += add_lines;
			(*tb[h]).uf += add_lines;
		}

		uf = (*tb[n_f]).uf;
		qf_src += add_lines;
	}

#endif


if (gp_debug && w)
{
mprintf (z,"listado de lineas %d a %d   \n",pf,new_uf );
#if 0
for (i=pf; i<= new_uf; i++)
	mprintf (z,"(%3d)|%s|\n",i,(*fnp[i]).l);
#endif
mprintf (z,"salgo   \n" );
mprintf (z,"-----   \n\n" );
}

#endif
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_pri_l 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	busco_pri_l
 *	buscar la primera linea del bloque
 *	en el que esta la linea en cuestion 
 */

int	busco_pri_l(pf,uf,spt,v,mod_type)
int	pf,uf,spt,*v,*mod_type;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"busco_pri_l");
	w = g_dbg(z);

	for (i=spt, f_res=0, f_sigo=1 ; f_sigo && i>=pf; i--)
	{
if (gp_debug && w)
{
mprintf (z,"loop for con i: %d \n",i);
mprintf (z,"voy a strcpy  \n");
}

		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );

if (gp_debug && w)
{
mprintf (z,"voy a trim_end_f90 b1: |%s|  \n",b1);
}
		strcpy(b1, trim_end_f90(b1));

if (gp_debug && w)
{
mprintf (z,"loop for con i: %d |%s| \n",i,b1);
}
		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

if (gp_debug && w)
{
mprintf (z,"i: %d f_proceso: %d \n",i,f_proceso);
}
		if (f_proceso)
		{

if (gp_debug && w)
{
mprintf (z,"entro a tiene subrutina con |%s| \n",b1);
}
			if (tiene_subroutine(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
				*mod_type = 1;
			}

if (gp_debug && w)
{
mprintf (z,"sali de tiene subrutina con f_res %d |%s| \n",f_res,b1);
mprintf (z,"entro a tiene function  con |%s| \n",b1);
}
			if (tiene_function(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
				*mod_type = 2;
			}

if (gp_debug && w)
{
mprintf (z,"sali de tiene function  con f_res %d |%s| \n",f_res,b1);
}

		}
if (gp_debug && w)
{
mprintf (z,"pie de loop for ... i: %d\n",i);
}


	}


if (gp_debug && w)
{
mprintf (z,"f_res: %d \n",f_res);
mprintf (z,"salgo  ... \n");
mprintf (z,"----- \n");
}


	return (f_res);
}
		
	
/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_ult_l 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	busco_ult_l
 *	buscar la ultima linea del bloque
 *	en el que esta la linea en cuestion 
 */


int	busco_ult_l(pf,uf,spt,v,mod_type)
int	pf,uf,spt,*v,mod_type;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"busco_ult_l");
	w = g_dbg(z);

	for (i=spt, f_res=0, f_sigo=1 ; f_sigo && i<=uf; i++)
	{
		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		if (f_proceso)
		{
			if (tiene_end(b1,mod_type) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
			}
		}
	}


	return (f_res);
}
		
	
/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_ult_u 
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	busco_ult_u
 *	buscar la ultima linea del bloque
 *	que tenga un use 
 *	en el que esta la linea en cuestion 
 */


int	busco_ult_u(pf,uf,spt,v)
int	pf,uf,spt,*v;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"busco_ult_u");
	w = g_dbg(z);

	f_res = 0;
	*v = 0;

	for (i=pf, f_sigo=1 ; f_sigo && i<=uf; i++)
	{
		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
		strcpy(b1, trim_end_f90(b1));

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		if (f_proceso)
		{

if (gp_debug && w)
{
	mprintf(z,"voy a tiene use con i: %d |%s| \n",i,b1);
}

			if (tiene_use(b1) )
			{
				f_res  = i;
				*v     = i;
if (gp_debug && w)
{
	mprintf(z,"encontre use en i: %d |%s| \n",i,b1);
}
			}
		}
	}

	return (f_res);
}
		
	

/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_use_allocate
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	busco_use_allocate
 *	buscar si ya puse use allocate_vars
 *	en el bloque del allocate que estoy viendo
 */


int	busco_use_allocate(pf,uf)
int	pf,uf;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];


	for (i=pf, f_res=0, f_sigo=1 ; f_sigo && i<=uf; i++)
	{
		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		if (f_proceso)
		{
			if (tiene_use_allocate(b1) )
			{
				f_res  = 1;
			}
		}
	}

	return (f_res);
}
		
	



/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_use_miscmod
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	busco_use_miscmod
 *	buscar si ya puse use allocate_vars
 *	en el bloque del allocate que estoy viendo
 */


int	busco_use_miscmod(pf,uf)
int	pf,uf;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"busco_use_miscmod");
	w = g_dbg(z);

	for (i=pf, f_res=0, f_sigo=1 ; f_sigo && i<=uf; i++)
	{
		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		if (f_proceso)
		{
if (gp_debug && w)
{
mprintf (z,"entro a tiene_miscmod  i: %d |%s| \n",i,b1);
}
			if (tiene_use_miscmod(b1) )
			{
				f_res  = 1;
			}
		}
	}

if (gp_debug && w)
{
mprintf (z,"f_res: %d \n",f_res);
mprintf (z,"------ \n\n");
}
	return (f_res);
}
		
	





/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_pri_d 
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	busco_pri_d
 *	buscar la primera linea del bloque
 *	que tenga una declaracion de variable
 *	en el que esta la linea en cuestion 
 */


int	busco_pri_d(pf,uf,spt,v)
int	pf,uf,spt,*v;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"busco_pri_d");
	w = g_dbg(z);

	*v = 0;

	for (i=spt, f_res=0, f_sigo=1 ; f_sigo && i<=uf; i++)
	{

		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		if (f_proceso)
		{
			if (tiene_logical_2(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
			}

			if (tiene_integer_2(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
			}

			if (tiene_real_2(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
			}

			if (tiene_character_2(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
			}

			if (tiene_allocatable_2(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
			}
		}
	}

	return (f_res);
}
		
	

/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_pri_s 
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	busco_pri_s
 *	buscar la primera linea del bloque
 *	que tenga un un SAVE
 */


int	busco_pri_s(pf,uf,spt,v)
int	pf,uf,spt,*v;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"busco_pri_s");
	w = g_dbg(z);

	*v = 0;

	for (i=spt, f_res=0, f_sigo=1 ; f_sigo && i<=uf; i++)
	{

		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
		strcpy(b1, trim_end_f90(b1));

if (gp_debug && w)
{
	mprintf(z,"busco    SAVE en: i %d |%s|\n",i,b1);
}

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		if (f_proceso)
		{
if (gp_debug && w)
{
	mprintf(z,"entre a f_proces: i %d |%s|\n",i,b1);
}
			if (tiene_save(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
if (gp_debug && w)
{
	mprintf(z,"encontre SAVE en: i %d |%s|\n",i,b1);
}

			}
		}
	}

	return (f_res);
}
		
	

	
/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_pri_i 
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	busco_pri_i
 *	buscar la primera linea con implicit none, si hay ...
 */


int	busco_pri_i(pf,uf,spt,v)
int	pf,uf,spt,*v;
{
	int	i;
	int	f_proceso;
	int	f_sigo;
	int	f_res;
	char	b1[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"busco_pri_i");
	w = g_dbg(z);

	*v = 0;

	for (i=spt, f_res=0, f_sigo=1 ; f_sigo && i<=uf; i++)
	{

		strcpy(b1, pasar_a_minusc( (*fnp[i]).l) );
		strcpy(b1, trim_end_f90(b1));

if (gp_debug && w)
{
	mprintf(z,"busco    IMPLICIT NONE en: i %d |%s|\n",i,b1);
}

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		if (f_proceso)
		{
if (gp_debug && w)
{
	mprintf(z,"entre a f_proces: i %d |%s|\n",i,b1);
}
			if (tiene_implicit(b1) )
			{
				f_sigo = 0;
				f_res  = 1;
				*v     = i;
if (gp_debug && w)
{
	mprintf(z,"encontre IMPLICIT NONE en: i %d |%s|\n",i,b1);
}

			}
		}
	}

	return (f_res);
}
		
	

	
	


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_multiple_vars
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * 	tiene_multiple_vars recibe un string donde todo el allocate fue
 *	metido en una sola linea 
 *	encuentra donde empieza y termina el area de allocate
 *	y devuele true si tiene mas de una variable allocada
 *	y false si es una sola variable
 *	tambien arma un vector de punteros a string,
 *	con cada variable allocada 
 */

int	tiene_multiple_vars(s)
char	*s;
{
	char	b0[MSTR];

	int	f_res;
	int	f_sigo;
	int	f_open_par;
	int	f_opt;

	int	p1,p2;
	int	i,j,k;
	int	l1,l2;
	int	c1,c2,c3;

	int	f1;

	int	st1,st2,st3,st4;

	char	z[MAXV];
	int	w;
	sprintf (z,"tiene_multiple_vars");
	w = g_dbg(z);

	/* copio string para trabajar local */
	strcpy(b0,s);
	l2 = strlen(b0);

	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	/* largo definitivo de la sentencia */
	l2 = strlen(b0);
	

	f_res = 0;
	p1    = 0;
	p2    = l2 -1;

	st1 = 0;
	st2 = 0;
	st3 = 0;

		
	/* la primer palabra tiene que ser allocate */
	st1 = 0;
	for (i=0, f_sigo=1 ; f_sigo && i<=p2; i++)
	{	if (b0[i] == ' ' || b0[i] == '\t')
			p1++;
		else
		{	
			if (!strncmp(b0+i,"allocate",8))
			{
				f_sigo = 0;
				st1 = 1;
				p1 = i+8;
			}
		}
	}
	
if (gp_debug && w)
{
mprintf (z,"paso 1 - primer palabra es allocate          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}


	/* puede haber blancos entre allocate y ( ... los quito */
	while (b0[p1] == ' ')
	{
		l2 = strlen(b0)-1;
		for (k=p1; k<l2; k++)
			b0[k] = b0[k+1];
		b0[l2]=0;
		p2--;
	}


if (gp_debug && w)
{
mprintf (z,"paso 2 - si hay blancos corro la linea          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b1          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}

	/* el ultimo caracter salvo blancos, es un parentesis que cierra */
	st2 = 0;
	l2 = strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i-- )
		if (b0[i] == ' ')
			p2--;
		else
		{
			if (b0[i] == ')')
			{
				f_sigo = 0;
				st2 = 1;
				p2  = i;
			}
			else
			{
				f_sigo = 0;
			}
		}

if (gp_debug && w)
{
mprintf (z,"paso 3 - el ultimo caracter salvo blanco es )           \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b1          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	/* tiene multiple vars : caracteres especificos de allocate entre ( y ) */
	st3 = 1;
	for (i=p1; i<=p2; i++)
	{
		if (!((b0[i] >= 'a' && b0[i] <= 'z') || 
		      (b0[i] >= '0' && b0[i] <= '9') || 
		      b0[i] == '=' ||						/* por lineas con el bendito stat= */
		      b0[i] == ' ' || b0[i] == ':' || b0[i] == ',' || 
		      b0[i] == '-' || b0[i] == '(' || b0[i] == ')' || 
		      b0[i] == '_' || b0[i] == '*' || b0[i] == '+' ||
		      b0[i] == '%'   )  )					/* por el bendito  calificador%variable ... */
		{
			st3 = 0;
		}
	}

	
if (gp_debug && w)
{
mprintf (z,"paso 4 - caracteres especificos entre ( y )          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b1          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}



	f_sigo = 1;
	c2  = 0;
	c3  = 0;
	st4 = 0;
	i   = p1;

	while (f_sigo)
	{
		f_opt = 1;

		if (f_opt && c2 == 0 && b0[i] == '(' )
		{
			c2 = 1;
			f_opt = 0;
		}

		if (f_opt && c2 >= 1 && b0[i] == '(' )
		{
			c2++;
			f_opt = 0;
		}

		if (f_opt && c2 > 1 && b0[i] == ')' )
		{
			c2--;
			f_opt = 0;
		}

		if (f_opt && c2 == 1 && b0[i] == ',')
		{
			c3++;
			f_opt = 0;
		}
 
		i++;

		if (i> p2)
		{
			f_sigo = 0;
		}

	}

	if (c3)
	{
		st4 = 1;
		
		if (tiene_stat(b0) && c3 == 1 )
			st4 = 0;
	}

if (gp_debug && w)
{
mprintf (z,"paso 5 - hay comas sin estar entre    ( y )  \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b1          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d,%d| \n",st1,st2,st3,st4);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	if (st1 && st2 && st3 && st4)
		f_res = 1;

	
			
	
if (gp_debug && w)
{
mprintf (z,"paso 5           \n");
mprintf (z,"f_res      |%d| \n",f_res);
mprintf (z,"----- \n");
}


if (gp_debug && w)
{
	if (f_res == 0)
		mprintf (z,"f .. f_res %d s: |%s|\n",f_res,s);

	if (f_res == 1)
		mprintf (z,"f .. f_res %d s: |%s|\n",f_res,s);
}


if (gp_debug && w)
{
mprintf (z,"----- \n");
}

	return (f_res);

}





/*
 * -----------------------------------------------------------------------------------
 *
 *	trim_beg 
 *
 * -----------------------------------------------------------------------------------
 */


char	*trim_beg(s)
char	*s;
{
	static	char	b0[MAXB];
	int	i,j,k;
	int	p1;
	int	l1;
	int	f_sig;

	l1=strlen(s);

	p1 = 0;

	for (i=0, f_sig=1; f_sig && i<l1; i++)
		if (s[i] != ' ' && s[i] != '\t' && s[i] != '+')
			p1=i, f_sig=0;

	strcpy(b0,s+p1);
	
	return (b0);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	trim_beg_f90 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * recorta blancos y tabs al comienzo de la linea
 *
 */

char	*trim_beg_f90(s)
char	*s;
{
	static	char	b0[MAXB];
	int	i,j,k;
	int	p1;
	int	l1;
	int	f_sig;

	l1=strlen(s);

	p1 = 0;

	for (i=0, f_sig=1; f_sig && i<l1; i++)
		if (s[i] != ' ' && s[i] != '\t' )
			p1=i, f_sig=0;

	strcpy(b0,s+p1);
	
	return (b0);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	trim_end 
 *	recorta comentarios al final de la linea 
 *
 * -----------------------------------------------------------------------------------
 */


char	*trim_end(s)
char	*s;
{
	static	char	b0[MAXB];
	int	i,j,k;
	int	p1;
	int	l1,l2;
	int	f_sigo;

	l2=strlen(s);
	strcpy(b0,s);


	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}
	
	return (b0);
}
	
/*
 * -----------------------------------------------------------------------------------
 *
 *	trim_end_f90 
 *	recorta comentarios al final de la linea 
 *
 * -----------------------------------------------------------------------------------
 */


char	*trim_end_f90(s)
char	*s;
{
	static	char	b0[MSTR];
	int	i,j,k;
	int	p1;
	int	l1,l2;
	int	f_sigo;
	int	f_proceso;

	l2=strlen(s);
	strcpy(b0,s);


	f_proceso = 1;
#if 0
	/*
	 * es_linea_comentario aca no sirve ...
	 * analiza cosas como |ccr_esta_variable ... | y la marca como comentario 
	 */

	if (linea_vacia(b0) || es_linea_comentario(b0))
		f_proceso = 0;
#endif
	if (linea_vacia(b0) )
		f_proceso = 0;


	if (f_proceso)
	{

	/* recorto si hay comentarios al final de la linea
	 * hay un solo caso con allocate  ... !real *4! .... 
	 */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	l2=strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '&')
			f_sigo=0, b0[i] = 0;
	}

	}
	

	return (b0);
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	chanchada
 *
 *
 * -----------------------------------------------------------------------------------
 */

char	*chanchada(s)
char	*s;
{
	static	char	b0[MSTR];
	int	i,j,k;
	int	l1,l2;

	char	z[MAXV];
	int	w;
	sprintf (z,"chanchada");
	w = g_dbg(z);

	strcpy(b0,s);
	l2=strlen(b0);

	for (i=0; i<l2; i++)
		if (b0[i] == '&')
			b0[i] = ' ';

	return (b0);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	chanchada_for
 *
 *
 * -----------------------------------------------------------------------------------
 */

char	*chanchada_for(s)
char	*s;
{
	static	char	b0[MSTR];
	int	i,j,k;
	int	l1,l2;
	int	f_sig;

	char	z[MAXV];
	int	w;
	sprintf (z,"chanchada_for");
	w = g_dbg(z);

	strcpy(b0,s);
	l2=strlen(b0);

	i=0;
	while (b0[i] == ' ' || b0[i] == '\t')
		i++;

	if (b0[i] == '+')
		b0[i] = ' ';


	return (b0);
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_allocate 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	pending ...
 *	habria que mirar solo en la sentencia sin comentario al fondo ....
 *	esta fallando por "deallocated at bottom" .... (econintr ... )
 *
 *      ALLOCATE(BLOCK_DISP_COST(NUNITS,2)) ! DEALLOCATED AT BOTTOM
 *
 */


int	tiene_allocate(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;
	int	k2_key;

	int	k1,k2;

	char	z[MAXV];
	int	w;
	sprintf (z,"tiene_allocate");
	w = g_dbg(z);

	k1 = 8;
	k2 = 0;
	l1 = strlen(s);

if (gp_debug && w)
{
mprintf(z,"voy a ver si tiene_allocate |%s| \n",s);
}


	for (i=0, f_res=0; i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate",k1))
		{
			k2_key = 0;
			f_try = 1;

			k2++;
			if (f_try && s[i+k1] == 'd' )
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && s[i+k1] == 'a' )
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && s[i+k1] == '_' )
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_deallocate(s))
			{	f_try = 0, k2_key=k2;
				if (tiene_deallocated_at(s))
					f_try = 1;
			}

			k2++;
			if (f_try && tiene_call(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_pentry(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene__allocate(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_er_message(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_amazingly(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_3possible(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_allocatex(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_remainder(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_proport(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_room(s))
				f_try = 0, k2_key=k2;

			k2++;
			if (f_try && tiene_arrays(s))
				f_try = 0, k2_key=k2;

if (gp_debug && w)
{
mprintf(z,"voy a ver si tiene_allocate f_try: %d k2_key: %d |%s| \n",f_try,k2_key,s);
}
			if (f_try)
			{	f_res=1;
#if 0
				f_sig=0;
#endif
			}
		}
	}

	return (f_res);
}
	





int	tiene_deallocate(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"deallocate",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_deallocated_at(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 16;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"deallocated at b",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_call(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 4;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"call",k1))
		{
			f_try = 1;

			if (!strncmp(s+i,"calls",k1+1))
				f_try = 0;

			if (!strncmp(s+i,"call_",k1+1))
				f_try = 0;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_pentry(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 5;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"entry",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


int	tiene__allocate(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 9;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"_allocate",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


int	tiene_er_message(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"er_message",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_amazingly(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 9;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"amazingly",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}




int	tiene_3possible(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"3 possible",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_allocatex(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate x",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_remainder(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 18;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate remainder",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_proport(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 16;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate proport",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_room(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 13;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate room",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_arrays(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 15;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocate arrays",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}









int	tiene_string(s,p)
char	*s,*p;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	l1 = strlen(s);
	k1 = strlen(p);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,p,k1))
		{
			f_try = 1;

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}









/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_stat
 *
 *	chequea si ya se incluyo en el file un stat= ... 
 *
 * -----------------------------------------------------------------------------------
 */



int	tiene_stat(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 5;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"stat=",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


/*
 *	chanchada ... !
 *	hacer una unica rutina ...
 *	tiene que esar str en linea, 
 *	si linea no es comentario,
 *	si str no esta en comentario al fondo 
 *	etc
 */


int	tiene_subroutine(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 10;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"subroutine",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_function(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 8;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"function",k1))
		{
			f_try = 1;

			if (f_try && tiene_string(s,"_function"))
				f_try = 0;

			if (f_try && tiene_string(s,"function_"))
				f_try = 0;

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


/*
 *	tiene_end
 *	tiene que encontrar un :
 *	end
 *	end [ function / subroutine ]
 */

int	tiene_end(s,mod_type)
char	*s,mod_type;
{
	static	char	b0[MAXB];

	int	i,j,k,l1,l2;
	int	f_res,f_sig,f_try;
	int	f_t1,f_t2;
	int	k1;

	char	z[MAXV];
	int	w;
	sprintf (z,"tiene_end");
	w = g_dbg(z);

	strcpy(b0,s);
	strcpy(b0,trim_end(b0));

	k1 = 3;
	l1 = strlen(b0);


	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1 + 1; i++)
	{	

		if (!strncmp(b0+i,"end",k1))
		{
			f_try = 1;
		

			if (f_try)
			{
 				for (k=i+3; k<l1; k++)
				{	
					if (b0[k] != ' ' && b0[k] != '\t' && b0[k] != 0 )
						f_try = 0, f_sig = 0;
				}
			}

			if (f_try)
			{
				for (k=0; k<l1; k++)
					if (!strncmp(b0+k,"dividend",8))
						f_try = 0, f_sig = 0;
			}


			if (f_try)
				f_res=1, f_sig=0;
		}

		if (mod_type == 1)
		{
			f_t1 = 0;
			if (!strncmp(b0+i,"end subroutine",14))
				f_t1 = 1;

			if (f_t1)
				f_res=1, f_sig=0;
		}

		if (mod_type == 2)
		{
			f_t2 = 0;
			if (!strncmp(b0+i,"end function",12))
				f_t2 = 1;

			if (f_t2)
				f_res=1, f_sig=0;
		}

	}

	/* este caso no cuenta */
	if (!strncmp(b0+i,"end do",6))
		f_res = 0;

if (gp_debug && w)
{
	if (f_res)
	{
		mprintf (z,"encontre end. s : |%s| \n",s);
		mprintf (z,"encontre end. b0: |%s| \n",b0);
	}
}

	return (f_res);
}


/*
 *	tiene_use
 *	tiene que encontrar un use con algun string valido ...
 */

int	tiene_use(s)
char	*s;
{
	int	i,j,k,l1,l2;
	int	f_res,f_sig,f_try;

	int	k1;

	char	z[MAXV];
	int	w;
	sprintf (z,"tiene_use");
	w = g_dbg(z);

	k1 = 4;
	l1 = strlen(s);


	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1 + 1; i++)
	{	

		if (!strncmp(s+i,"use ",k1))
		{
			f_try = 1;
		

			for (k=i+3; k<l1; k++)
			{	

				if (gp_debug && w)
				{
					mprintf (z,"k %2d s[k] %c %2d \n",k,s[k],s[k]);
				}

				if (!( s[k] == ' ' || 
				       s[k] == '_' || 
				      (s[k] >= 'a' && s[k] <= 'z') || 
				      (s[k] >= '0' && s[k] <= '9')     ) )

					f_try = 0, f_sig = 0;
			}

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


int	tiene_integer_2(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 8;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"integer ",k1))
		{
			f_try = 1;

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_logical_2(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 8;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"logical ",k1) || !strncmp(s+i,"logical*",k1) )
		{
			f_try = 1;

			if (f_try)
			{
				if (!strncmp(s+i,"logical function ",17))
					f_try = 0;
			}

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


int	tiene_real_2(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 4;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"real",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_character_2(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 9;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"character",k1))
		{
			f_try = 1;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



int	tiene_allocatable_2(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 11;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1; i++)
	{	
		if (!strncmp(s+i,"allocatable",k1))
		{
			f_try = 1;

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}





int	tiene_use_allocate(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	char	z[MAXV];
	int	w;
	sprintf (z,"tiene_use_allocate");
	w = g_dbg(z);

	k1 = 17;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1+1; i++)
	{	


		if (!strncmp(s+i,"use allocate_vars",k1))
		{
if (gp_debug && w)
{
	mprintf (z,"comparo:  |%s| %d chars \n",s+i,k1);
	mprintf (z,"con    :  |%s| %d chars \n","use allocate vars",k1);
}
			f_try = 1;

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}




int	tiene_save(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 4;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1 + 1; i++)
	{	
		if (!strncmp(s+i,"save",k1))
		{
			f_try = 1;

			if (f_try)
				if (!strncmp(s+i,"save_",k1+1))
					f_try = 0, f_res = 0, f_sig = 0;


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}


int	tiene_implicit(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 13;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1 + 1; i++)
	{	
		if (!strncmp(s+i,"implicit none",k1))
		{
			f_try = 1;

#if 0
			/* excepciones ... */
			if (f_try)
				if (!strncmp(s+i,"save_",k1+1))
					f_try = 0, f_res = 0, f_sig = 0;
#endif


			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_use_miscmod
 *
 *	busco use miscmod en sentencia
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	tiene_use_miscmod
 *	busca si en la linea de sent ya puse use miscmod
 *
 *	mejoras !
 *	deberia encontrar use miscmod 
 *	sin importar blancos entre use y miscmod
 */

int	tiene_use_miscmod(s)
char	*s;
{
	int	i,j,k,l1;
	int	f_res,f_sig,f_try;

	int	k1;

	k1 = 11;
	l1 = strlen(s);

	for (i=0, f_res=0, f_sig=1; f_sig && i < l1 - k1+1; i++)
	{	
		if (!strncmp(s+i,"use miscmod",k1))
		{
			f_try = 1;

			if (f_try)
				f_res=1, f_sig=0;
		}
	}

	return (f_res);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	cantidad_de_vars
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	cantidad_de_vars(s,qv)
char	*s;
int	*qv;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	ql;
	int	ptr;
	int	stp;
	int	l1,l2;
	int	c1,c2;
	int	p1,p2;

	char	b0[MSTR];
	char	b1[MSTR];
	char	b2[MSTR];

	char	d0[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"cantidad_de_vars");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	for (i=0; i<MAX_VARS; i++)
		gv_str[i][0]=0;

	ql=0;

	for (j=0, k=0, f1=0, l2=strlen(s)-1; !f1 && j<l2; j++)
		if (s[j] != ' ')
			k=j, f1 = 1;

	strcpy(b0,s+k);

	/* todavia se cuelan los & */
	for (j=0, k=0, f1=0, l2=strlen(s)-1; !f1 && j<l2; j++)
		if (b0[j] == '&')
			b0[j] = ' ';


if (gp_debug && w)
{
	printf ("entro a cant de vars \n");
	printf ("s |%s|\n",s);
	printf ("b0|%s|\n",b0);
}

	/* limpiamos el line feed */
	for ( f1=0, j=strlen(b0)-1; !f1 && j >= 0; j--)
		if (b0[j] == '\n')
			b0[j]=0,f1=1;

if (gp_debug && w)
{
	printf ("desp de limpiar line feed  \n");
	printf ("s |%s|\n",s);
	printf ("b0|%s|\n",b0);
}

	/* sacamos el ultimo | */
	strcpy(b1,b0);
	for ( f1=0, j=strlen(b1); !f1 && j >= 0; j--)
		if (b1[j] == '|')
			b1[j]=0,f1=1;

	strcpy(b2,b1);

if (gp_debug && w)
{
	printf ("desp de sacar el ultimo |  \n");
	printf ("s |%s|\n",s);
	printf ("b0|%s|\n",b0);
	printf ("b1|%s|\n",b1);
	printf ("b2|%s|\n",b2);
}

	/* sacamos todos los blancos */
	ptr = 0;
	do
	{
		if (b1[ptr] == ' ')
		{
			l2 = strlen(b1)-1;
			for (k=ptr; k<l2; k++)
			{	b1[k] = b1[k+1];
			}
			b1[l2]=0;
		}
		else
			ptr++;

	} while (b1[ptr]);

if (gp_debug && w)
{
	printf ("desp de sacar blancos  \n");
	printf ("s |%s|\n",s);
	printf ("b0|%s|\n",b0);
	printf ("b1|%s|\n",b1);
	printf ("b2|%s|\n",b2);
}

	/* sacamos los comentarios al final */
	for ( f1=0, j=strlen(b1)-1 ; !f1 && j > 9 ; j--)
		if (b1[j] == '!')
			b1[j]=0;


if (gp_debug && w)
{
	printf ("desp de sacar comentarios al final   \n");
	printf ("s |%s|\n",s);
	printf ("b0|%s|\n",b0);
	printf ("b1|%s|\n",b1);
	printf ("b2|%s|\n",b2);
}

	/* sacamos el stat si tiene, guardamos registro de que tiene */
	memset(d0,0,sizeof(d0));
	for ( f1=0, j=strlen(b1)-1 ; !f1 && j > 9; j-- )
		if (!strncmp(b1+j,",stat=",6))
		{	f1=1;
			strcpy(d0,b1+j+1);
			d0[strlen(d0)-1]=0;
			b1[j] = ')';
			b1[j+1] = 0;
		}


if (gp_debug && w)
{
	printf ("desp de sacar stat, si tiene   \n");
	printf ("s |%s|\n",s);
	printf ("b0|%s|\n",b0);
	printf ("b1|%s|\n",b1);
	printf ("b2|%s|\n",b2);
	printf ("d0|%s|\n",d0);
	printf ("f1 %d \n",f1);
}

#if 0
     allocate(var1,var2)
     allocate(var1(xxx1),var2(xxx2))
#endif


	/* intentamos contar variables ... */
	stp = 0;
	for (j=8 , l2=strlen(b1), c1 = 0, c2= 1 ; j < l2;  j++)
	{
		if (b1[j] == '(' )
		{	c1++;
			if (c1 == 1)
				p1 = j+1;
		}
		
		if (b1[j] == ')' )
		{	c1--;
			if (c1 == 1)
			{	p2 = j-1;

				/* encontre var de tipo var( ..... ) */
				memset(gv_str[stp],0,MAXB);
				strncpy(gv_str[stp],b1+p1,p2-p1+2);
				stp++;
			}
		}

		

		if (b1[j] == ',' && c1 == 1)
		{	
			c2++;
			p1 = j+1;

		}
	}




if (gp_debug && w)
{
	printf ("%04d,|%s|\n",ql,b2);
	printf ("%04d,|%s| - |%s| (%03d) \n",ql,b1,d0,c2);

	for (j=0; j<c2; j++)
		printf ("--%03d |%s|\n",j,gv_str[j]);

	printf ("\n");
}

	*qv = c2;


	/* resumen del proceso */
	if (gp_fverbose("d3"))
	{
		printf ("s len: (%03d)  str: |%s|\n",strlen(s),s);
		printf ("q var: (%03d) \n",c2);
		for (j=0; j<c2; j++)
			printf ("--%03d |%s|\n",j,gv_str[j]);
		printf (" ... \n");
	}

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


	/* hasta que encontremos como chequear ... */
	return(1);
}















/*
 * -----------------------------------------------------------------------------------
 *
 *	ex9_p2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec9
 * hace algo con todas las lineas cargadas en memoria
 *
 * prueba de mover toda las lineas 
 *
 */



int	ex9_p2()
{
	static	int	f_miscmod1 = 0;
	static	int	f_miscmod2 = 0;

	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	tipo_ext;
	int	add_lines;		/* cant de lineas que hay que sumar al fuente al corregir un allocate */
	int	num_alloc;		/* numero de alloc encontrado en src */
	int	num_alloc_key;		/* numero de key usado en file para identificar alloc en fuente */
	int	num_alloc_fnd;		/* num de alloc cuando es encontrado en src */
	int	nf_check;		/* linea donde esta el check alloc encontrado en el fuente */
	int	num_f;
	int	q_lin;

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"ex9_p2");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;


	if (gp_debug && w)
	{
		mprintf (z,"Empiezo mover bulk \n");
		mprintf (z,"Cantidad de archivos cargados:   %5d \n",qf_ff);
		mprintf (z,"Cantidad de lineas   cargadas:   %5d \n",qf_src);
		
		for (i=0; i<qf_ff; i++)
		{
			mprintf (z,"%3d  de %6d a %6d  |%s| \n",i,(*tb[i]).pf,(*tb[i]).uf,(*tb[i]).n );
		}

		mprintf (z,"----------\n\n");
	}


	num_f = 3;
	q_lin = 3;

	mover_bulk(num_f,q_lin);

	if (gp_debug && w)
	{
		mprintf (z,"Termine mover bulk \n");
		mprintf (z,"Cantidad de archivos cargados:   %5d \n",qf_ff);
		mprintf (z,"Cantidad de lineas   cargadas:   %5d \n",qf_src);
		
		for (i=0; i<qf_ff; i++)
		{
			mprintf (z,"%3d  de %6d a %6d  |%s| \n",i,(*tb[i]).pf,(*tb[i]).uf,(*tb[i]).n );
		}

		mprintf (z,"----------\n\n");
	}




}


/*
 * -----------------------------------------------------------------------------------
 *
 *	mover_bulk
 *
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	num_f es el numero de file desde donde quiero mover
 *	qlin  es el numero de lineas que vamos a mover
 */

int	mover_bulk(num_f,qlin)
int	num_f;
int	qlin;
{
	int 	i,j,k;
	int	pf,uf;
	int	ln;
	int	new_uf;

	char	z[MAXV];
	int	w;
	sprintf (z,"mover_bulk");
	w = g_dbg(z);


	/* no se puede mover desde num_f igual a 0 */
	if (num_f == 0 )
		error(703);
#if 1
	if (num_f > qf_ff )
		error(704);
#endif

	if (num_f >= 1 && num_f < qf_ff)
	{

	pf = (*tb[num_f]).pf;
	uf = (*tb[qf_ff-1]).uf;
	new_uf = uf + qlin;

	for (i=1; i<=qlin; i++)
	{
		k = uf+i;

if (gp_debug && w)
{
mprintf (z,"agrego linea al final k %d   \n",k);
}

		/* agrego lineas al final */
		fnp[k] = (fnptr  ) malloc (sizeof (node));
		if ( fnp[k] == NULL)
			error(918);

		(*fnp[k]).l[0] = 0;
		(*fnp[k]).f1 = 0;
		(*fnp[k]).f2 = 0;
		(*fnp[k]).f3 = 0;
	}


if (gp_debug && w)
{
mprintf (z,"corro lin desde %d a %d tot %d lin   \n",new_uf,pf, new_uf - pf   +1  );

}

	for (j=new_uf; j >= pf; j-- )
	{
		memcpy ( fnp[j],fnp[j-qlin], sizeof (node) );
	}


	/* actualizo la tabla de pf uf */
	for (i= num_f; i< qf_ff; i++)
	{
		(*tb[i]).pf += qlin;
		(*tb[i]).uf += qlin;
	}
	
	(*tb[num_f-1]).uf += qlin;
	qf_src += qlin;


if (gp_debug && w)
{
mprintf (z,"vacio lin desde %d a %d tot %d lin   \n",pf-qlin, pf-1, qlin   );
}
	/* vacio las lineas que quedaron disponibles */
	for (i= (*tb[num_f-1]).uf ; i > (*tb[num_f-1]).uf - qlin ; i--)
	{
		strcpy ( (*fnp[i]).l, "esta_linea_vacia");
		(*fnp[i]).f1 = 0;
		(*fnp[i]).f2 = 0;
		(*fnp[i]).f3 = 0;
	}


if (gp_debug && w)
{
mprintf (z,"salgo de mover_bulk    \n",pf-qlin, pf-1, qlin   );
}


	} /* if (num_f >= 1 && num_f < qf_ff ... */



	if (num_f == qf_ff)
	{

	pf = (*tb[num_f-1]).pf;
	uf = (*tb[qf_ff-1]).uf;
	new_uf = uf + qlin;

	for (i=1; i<=qlin; i++)
	{
		k = uf+i;

if (gp_debug && w)
{
mprintf (z,"agrego linea al final k %d   \n",k);
}

		/* agrego lineas al final */
		fnp[k] = (fnptr  ) malloc (sizeof (node));
		if ( fnp[k] == NULL)
			error(918);

		(*fnp[k]).l[0] = 0;
		(*fnp[k]).f1 = 0;
		(*fnp[k]).f2 = 0;
		(*fnp[k]).f3 = 0;
	}

#if 0
	for (j=new_uf; j >= pf; j-- )
	{
		memcpy ( fnp[j],fnp[j-qlin], sizeof (node) );
	}
#endif

	/* actualizo numeros */
	(*tb[num_f-1]).uf += qlin;
	qf_src += qlin;

	/* vacio las lineas que quedaron disponibles */
	for (i= (*tb[num_f-1]).uf ; i > (*tb[num_f-1]).uf - qlin ; i--)
	{
		strcpy ( (*fnp[i]).l, "esta_linea_vacia");
		(*fnp[i]).f1 = 0;
		(*fnp[i]).f2 = 0;
		(*fnp[i]).f3 = 0;
	}



	} /* if (num_f == qf_ff ... */


}







/*
 * -----------------------------------------------------------------------------------
 *
 *	ex9_p3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec9
 * hace algo con todas las lineas cargadas en memoria
 *
 */



int	ex9_p3()
{
	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4;
	int	f1,f2,f3,f4,f5;
	int	f_proceso;
	int	f_stat;
	int	f_keep;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	tipo_ext;
	int	add_lines;		/* cant de lineas que hay que sumar al fuente al corregir un allocate */
	int	num_alloc;		/* numero de alloc encontrado en src */
	int	num_alloc_key;		/* numero de key usado en file para identificar alloc en fuente */
	int	num_alloc_fnd;		/* num de alloc cuando es encontrado en src */
	int	nf_check;		/* linea donde esta el check alloc encontrado en el fuente */
	int	r1;
	int	k_amp;
	
	int	tipo09;
	int	tipo11;
	int	tipo13;
	int	tipo15;

	char	m0[MSTR];
	char	m1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"ex9_p3");
	w = g_dbg(z);

	memset(b4,'X',MAXB);
	memset(m0,0,MSTR);
	strcpy(base_name,"empty");

	tipo_ext = 0;
	c2 = 0;
	c3 = 0;
	c4 = 0;



	i = 0;
	f_keep = 1;

	while (f_keep)
	{
		strcpy(b0,(*fnp[i]).l );

if (gp_debug && w)
{
	mprintf (z,"FOR prin (%3d) |%s| \n",i,b0);
	mprintf (z,"... \n\n");
}


		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));
		strcpy(exte_name,e_name(i));

		if (!strncmp(exte_name,"for",3))
			tipo_ext = 1;

		if (!strncmp(exte_name,"f90",3))
			tipo_ext = 2;

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}



		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
			num_alloc = 0;
			num_alloc_key = 1;

if (1)
{
printf ("Trabajo con ... (%s) |%s|\n",exte_name,prog_name);
}

		}


		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1, pasar_a_minusc(b0));
		strcpy(b2, pasar_a_minusc(b0));


		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;


		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			strcpy(b0,trim_end_f90(b0));
			strcpy(b0,trim_blanks(b0));
			l2 = strlen(b0);

			strcpy(b4,"_");
			if (l2 > 83)
				strcpy(b4,"L");

			fprintf (hfout,"%s %-30.30s |%s|\n",b4,prog_name,b0);

		} /* proceso */


		strcpy( (*fnp[i]).l , b0 );

		i++;
		if (i >= qf_src)
			f_keep = 0;

	} /* for */

}





/*
 * -----------------------------------------------------------------------------------
 *
 *	ex9_p4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec9
 * hace algo con todas las lineas cargadas en memoria
 *
 * prueba de combinar hacer_lugar (version multi file)
 * con mover_bulk
 *
 */



int	ex9_p4()
{
	int 	h,i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2,c3,c4;
	int	f1,f2,f3,f4,f5;
	int	p1,p2,p3,p4;
	int	f_proceso;
	int	f_stat;
	int	f_keep;
	int	f_act;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	exte_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	int	pf,uf,nf;
	int	tipo_ext;
	int	add_lines;		/* cant de lineas que hay que sumar al fuente al corregir un allocate */
	int	nf_check;		/* linea donde esta el check alloc encontrado en el fuente */
	int	r1;
	int	n1,n2;
	
	char	z[MAXV];
	int	w;
	sprintf (z,"ex9_p4");
	w = g_dbg(z);


	/*
	 *	en esta version ...
	 *	recorro todas las lineas de fnp 
	 *	y bajo detemrinada condicion, busco insertar 3 lineas 
	 *	en el file
	 */

	strcpy(base_name,"empty");
	i = 0;
	f_keep = 1;
	f_act  = 0;

	k2 = 0;

	while (f_keep)
	{

		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));
		strcpy(exte_name,e_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
if (1)
{
printf ("Trabajo con ... %03d (%s) |%s|\n",n_f,exte_name,prog_name);
}
		}


		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1, pasar_a_minusc(b0));


		l2 = strlen(b1);

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			if (tiene_allocate(b1))
			{
if (gp_debug && w)
{
	mprintf (z,"Programa        : |%s|\n",prog_name);
	mprintf (z,"Extension       : |%s|\n",exte_name);
	mprintf (z,"Numero de linea :  %5d\n",i);
	mprintf (z,"Numero de file  :  %5d\n",n_f);
	mprintf (z,"Primera linea   :  %5d\n",pf);
	mprintf (z,"Ultima  linea   :  %5d\n",uf);
	mprintf (z,"qf_ff           :  %5d\n",qf_ff);
	mprintf (z,"qf_src          :  %5d\n",qf_src);
	mprintf (z,"....... \n");
}

				/* hacemos la movida 
				 * esto diria:
				 * quiero 3 lineas nuevas, en el archivo nro tb n_f , a partir de la linea i 
				 */

				/* probemos el primero */
				if (k2 == 2 || 1)
				{
#if 0
					n_f = n_name(i);
					mover_bulk(n_f+1,3);
#endif

#if 1
					hacer_lugar_mf (i,3);
#endif

					i+= 3;
				}


				k2++;
			}

		} /* proceso */



		i++;
		if (i >= qf_src)
			f_keep = 0;

	} /* for */


}




/*
 * -----------------------------------------------------------------------------------
 *
 *	v_pars
 *
 *	parser de lineas, devolviendo variables ... en lugar de tokens 
 *
 * -----------------------------------------------------------------------------------
 */

void v_pars(s, n, vn, vp) 
char	*s;
int	*n;
int	*vn;
char	**vp;
{

    int start;
    int len;
    int i;

    // Inicializar el contador de variables
    *n = 0; 
    len = strlen(s);
    i = 0;

    while (i < len) 
    {
	// Saltar espacios en blanco
	while (i < len && isspace(s[i])) 
	{
	    i++;
	}

	// Si encontramos una letra, es el inicio de una variable
	if (i < len && isalpha(s[i])) 
	{
	    // Guardar el puntero al inicio de la variable
	    vp[*n] = &s[i]; 
	    start = i;

	    // Avanzar hasta el final de la variable
	    while (i < len && (isalnum(s[i]) || s[i] == ' ' || s[i] == '_')) 
	    {
		i++;
	    }

	    // Guardar la longitud de la variable
	    vn[*n] = i - start; 

	    (*n)++; 
	} 
	else 
	{
	    i++;
	}
    }



	printf ("v_pars .... \n");
	printf ("n:    %2d \n",*n);
	for (i=0; i< *n; i++)
	{
		printf ("i: %2d  len: %2d  str: |%s| \n",i,vn[i],vp[i]);
	}

}



/*
 * -----------------------------------------------------------------------------------
 *
 *	ex8_p2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec8
 * hace algo con todas las lineas cargadas en memoria
 *
 *
 */

int	ex8_p2()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;

	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		strcpy (b2, pasar_a_minusc(b1));
		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);

		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

#if 0
							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;

							}
#endif
						}
					}
				}		
	
			} /* while f5 */
		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



}







/*
 * -----------------------------------------------------------------------------------
 *
 *	ex8_p3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * llamado por pro_exec8
 * hace algo con todas las lineas cargadas en memoria
 *
 *
 */

int	ex8_p3()
{
	int 	i,j,k,k1,k2;
	int	l1,l2;
	int	c1,c2;
	int	f1,f2,f3,f4,f5;
	int	p1,p2;
	int	f_proceso;
	int	n_f;
	char	base_name[MAXV];
	char	prog_name[MAXV];
	char	b0[MAXB];
	char	b1[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	int	pf,uf,nf;
	int	qv1,qv2;
	int	qm1,qm2,qm3;
	int	n_var;

	static char	b2[MAXB];

	strcpy(base_name,"empty");

	qv1 = 0;
	qm1 = 0;

	/* para todas las lineas */
	for (i=0; i < qf_src; i++)
	{
		/* me fijo que archivo es */
		strcpy(prog_name,f_name(i));

		/* me fijo lineas */
		for (j=0, f1=1, n_f=0; f1 && j<qf_ff; j++)
			if ( i >= (*tb[j]).pf && i <= (*tb[j]).uf )
			{	n_f = j;
				pf = (*tb[j]).pf;
				uf = (*tb[j]).uf;
				f1=0;
			}

		if (strcmp(base_name,prog_name))
		{	strcpy(base_name,prog_name);
			nf = 0;
		}

		/* proceso linea i */
		strcpy(b0,(*fnp[i]).l );
		strcpy(b1,(*fnp[i]).l );

		/* trabajemos con minusculas */
		strcpy (b2, pasar_a_minusc(b1));


		if ( !es_linea_comentario(b2) && tiene_coment_final (b2,&p1))
		{
			b2[p1] = 0;
		}
		l2 = strlen(b2);


		f_proceso = 1;
		if (linea_vacia(b2) || es_linea_comentario(b2))
			f_proceso = 0;

		/* solo proceso lineas que no son comentario ni vacias */
		if (f_proceso)
		{	

			v_pars(b2,&n_var,v_size,v_name );

			printf ("VW0:  - - - - - - -\n");
			printf ("VW1:  b2     : |%s|\n",b2);
			printf ("VW2:  n_var  : %2d \n",n_var);
			printf ("VW3:  \n");
			for (j=0; j<n_var; j++)
			{
				printf ("VW4:  %2d %2d \n", j,v_size[j]);
				memset(b4,0,sizeof(b4));
				strncpy(b4,v_name[j],v_size[j]);
				printf ("VW5:  %2d %2d |%s| \n", j,v_size[j],v_name[j]);
				printf ("VW5:  %2d %2d |%s| \n", j,v_size[j],b4);
			}
			printf ("VW9:  \n");


#if 0
			for (i=0; i<n_var; i++)
			{
				memset(b4,0,sizeof(b4));
				strncpy(b4,v_name[i],v_size[i]);
				fprintf (hfout,"%30.30s %2d |%s| \n",prog_name,i,b4);
			}
#endif



#if 0
			c1 = 0;
			f5 = 1;

			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}

							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("VV1 Prog:          |%s|\n",prog_name);
							printf ("VV2 var :          |%s|\n",b3);
							printf ("VV3 cnt : (%6d)\n",c1);
							printf ("VV4 lin : (%6d) |%s|\n",lne(nf),b0);
							printf ("VV5 lin : (%6d) |%s|\n",lne(nf),b1);
							printf ("VV6 \n");

#if 0
							if (fflog)
							{
								fprintf (hflog,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),b0);
								fprintf (hflog,"%-30.30s     %05d |%s|\n",prog_name,lne(nf),b1);
								fprintf (hflog,"%s","\n");
							}

							if (ffaux)
							{
								fprintf (hfaux,"%-30.30s (%d) %05d |%s|\n",prog_name,c1,lne(nf),vcb[k]);
							}

							if (ffout)
							{
								strcpy(vcb_o[qv1],vcb[k]);
								qv1++;
							}

							if (ffou4)
							{
								printf ("%4d %-30.30s |%s|\n",qm1,prog_name,vcb[k]);
								sprintf (vcb_p[qm1],"%-30.30s,|%s|",prog_name,vcb[k]);
     sprintf (vcb_c[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,vcb[k],40-strlen(vcb[k]), 40-strlen(vcb[k]), " " );
     sprintf (vcb_d[qm1],"%-30.30s,grep -i \"%s\" %-*.*s  ~/wrk/Midas/ABBICAP/*.* ",
	   prog_name,b5,40-strlen(b5), 40-strlen(b5), " " );

								qm1++;

							}
#endif
						}
					}
				}		
	
			} /* while f5 */
#endif

		} /* proceso */

		strcpy( (*fnp[i]).l , b1 );
		nf++;

	} /* for */



}












/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prueba 1
 *
 *	pruebas ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	ejercicios con estructuras, vectores, punteros
 */


#if 0
/*
 *	algo de referencia de las estrcturas y variables 
 */


int	st_load();			/* funcion de carga de listas */
int	q_st;                           /* cantidad de palabras en lista */

typedef	struct	tst	*stptr;
typedef	struct	tst
{	char	*wrd;
	int	num;
	struct	tst	*nx;
}	nodo_lista;

stptr	st_p1,st_p2,*st_n1,*st_n2,**st_nx;


typedef struct  tnode   *nodeptr;
typedef struct  tnode
{       char    *wrd;
	int     num;
	struct  tnode   *nx;
}       node;

nodeptr npx,np1,*npa;

			*npa = (nodeptr ) malloc ( sizeof (node));
			(**npa).wrd = ( char *) malloc(strlen(b1)+1);
			sprintf ( (**npa).wrd,"%s",b1);
			(**npa).num = q_wrd+1;
			(**npa).nx = (nodeptr) NULL;
			npa = (nodeptr *) & (*npa)->nx;


typedef	struct	tst	*stptr;
typedef	struct	tst
{	char	*wrd;
	int	num;
	struct	tst	*nx;
}	nodo_lista;

stptr	st_p1,st_p2,*st_n1,*st_n2,**st_nx;


#endif




int	ff1(stptr);
int	ff2(stptr *);
int	ff3(stptr **);
int	ff4(stptr **, stptr **);
int	ff5(stptr *);
int	ff6(stptr *, stptr *);
int	ff7(stptr **, stptr **);
int	ff8(stptr **);
int	ff9(stptr *);
int	ffa(stptr *);
int	qv;			/* cant de elementos en vector */

char	*sformat1 = "%03d   %-17s: %03d  -  %-17s: |%s| \n";
char	*sformat2 = "%03s   %-17s: %03d  -  %-17s: |%s| \n";
char	*sformat3 = "%03s   %-17s: %03d  -  %-17s: |%s| xx  %-17s: %03d  -  %-17s: |%s| \n";

/*
 *
 *	pruebas de manejo de estructuras
 *
 */

int	pro_prue1()
{


	struct tst	st1;			/* structura de tipo tst ... st1 */

	nlst		st2;                    /* uso el alias, nlst ... para crear st2 */
	nlst		*pst2;			/* puntero a struct tst o nslt */

	char		buf[64];

	stptr		v1[10];
	stptr		v2[10];

	int		i,j,k;

	char		b1[MAXB];
	int		flag,f1;
	int		ql;


	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra proceso prueba 1 \n\n",gp_tm());
	}

	qv = 5;		/* limitamos a 5 */


/*
 *
 *	1) Estructura simple
 *	
 *	usamos una estructura declarada
 *	la inicializamos
 *	imprimimos contenido
 *
 */


	printf ("1) Estructura simple \n");
	memset (&st1,0,sizeof(st1));
	memset (&st1,0,sizeof(struct tst));

	i=1;
	strcpy(buf,"e01_campo 1");

	st1.wrd = malloc (64);
	sprintf(st1.wrd,buf);
	st1.num = 1;

	printf (sformat1,
		i, "st1.num", st1.num, 
		   "st1.wrd", st1.wrd        );

	free (st1.wrd);
	printf (" - - - - \n\n");
	
/*
 *
 *	2) Puntero a estructura simple
 *	
 *	usamos un tipo a puntero a estructura
 *	allocamos memoria con malloc
 *	la inicializamos
 *	imprimimos contenido
 *
 */

	i=2;
	printf ("2) Puntero a estructura simple \n");
	st_p1 = malloc ( sizeof (struct tst) );

	(*st_p1).wrd = malloc (64);
	sprintf ( (*st_p1).wrd, "e02_campo 1");
	(*st_p1).num = 2;

	printf (sformat1,
		i, "(*st_p1).num", (*st_p1).num, 
		   "(*st_p1).wrd", (*st_p1).wrd        );


	free ( (*st_p1).wrd );
	free ( st_p1 );
	printf (" - - - - \n\n");

/*
 *
 *	3) Puntero a puntero de estructura simple
 *	
 *	usamos un tipo a puntero a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 */

	i=3;
	printf ("3) Puntero a puntero de estructura simple \n");
	st_p1 = malloc ( sizeof (struct tst) );
	(*st_p1).wrd = malloc (64);
	sprintf ( (*st_p1).wrd, "e03_campo 1");
	(*st_p1).num = 2;

	st_n1 = &st_p1;

	printf (sformat1,
		i, "(**st_n1).num", (**st_n1).num, 
		   "(**st_n1).wrd", (**st_n1).wrd        );

	free ( (*st_p1).wrd );
	free ( st_p1 );
	printf (" - - - - \n\n");

/*
 *
 *	4) Puntero a puntero de estructura simple + funcion1
 *	
 *	la funcion recibe un puntero a structura
 *
 *	usamos un tipo a puntero a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff1
 *	recibe un puntero a estructura
 *	imprime el contenido
 *
 */


	i=4;
	printf ("4) Puntero a puntero de estructura simple + funcion1 \n");

	st_p1 = malloc ( sizeof (nlst) );
	(*st_p1).wrd = malloc (64);
	sprintf ( (*st_p1).wrd, "e04_campo 11");
	(*st_p1).num = 11;

	st_p2 = malloc ( sizeof (nlst) );
	(*st_p2).wrd = malloc (64);
	sprintf ( (*st_p2).wrd, "e04_campo 21");
	(*st_p2).num = 21;

	st_n1 = &st_p1;
	printf (sformat1,
		i, "(**st_n1).num", (**st_n1).num, 
		   "(**st_n1).wrd", (**st_n1).wrd       );

	st_n1 = &st_p2;
	printf (sformat1,
		i, "(**st_n1).num", (**st_n1).num, 
		   "(**st_n1).wrd", (**st_n1).wrd       );

	ff1(st_p1);
	ff1(st_p2);

	printf (" - - - - \n\n");


/*
 *
 *	5) Puntero a puntero de estructura simple + funcion2
 *	
 *	la funcion recibe un puntero a puntero de stuctura
 *
 *	usamos un tipo a puntero a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff2
 *	recibe un puntero a estructura
 *	imprime el contenido
 */

	i=5;
	printf ("5) Puntero a puntero de estructura simple + funcion2 \n");

	st_n1 = &st_p1;
	ff2(st_n1);
	st_n1 = &st_p2;
	ff2(st_n1);

	printf (" - - - - \n\n");


/*
 *
 *	6) Vector de punteros a estructura simple
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero a a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=6;
	printf ("6) Vector de punteros a estructura simple  \n");

	for (j=0; j< qv; j++)
	{

		v1[j] = malloc ( sizeof (nlst ));
		memset (v1[j],0, sizeof (nlst ));
		(*v1[j]).wrd = malloc(64);
		sprintf ( v1[j]->wrd,"e06_campo %d",100+j);
		v1[j]->num = 100+j;

		printf (sformat1,
			i, "v1[j]->num", v1[j]->num, 
			   "v1[j]->wrd", v1[j]->wrd      );

		v2[j] = malloc ( sizeof (struct tst ));
		memset (v2[j],0, sizeof (struct tst ));
		(*v2[j]).wrd = malloc(64);
		sprintf ( v2[j]->wrd,"e06_campo %d",200+j);
		v2[j]->num = 200+j;

		printf (sformat1,
			i, "v2[j]->num", v2[j]->num, 
			   "v2[j]->wrd", v2[j]->wrd      );

	}

	printf ("\n");

	for (j=0; j< qv; j++)
	{
		ff1(v1[j]);
		ff1(v2[j]);
	}

	printf ("\n");

	for (j=0; j< qv; j++)
	{
		st_n1 = &v1[j];
		ff2(st_n1);
		st_n1 = &v2[j];
		ff2(st_n1);
	}

	printf (" - - - - \n\n");


/*
 *
 *	7) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=7;
	printf ("7) Pasaje de variables a funciones  \n");

	st_nx = &st_n1;

	st_n1 = &v1[0];
	ff3(st_nx);
	st_n1 = &v2[0];
	ff3(st_nx);


	printf ("\n");
	
	st_n1 = &v1[0];
	st_n2 = &v2[0];

	st_nx = &st_n1;
	st_ny = &st_n2;

	ff4(st_nx,st_ny);
	
	printf (" - - - - \n\n");



/*
 *
 *	8) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=8;
	printf ("8) Cadena de estructuras (lista de nodos )  \n");


	/* cantidad de palabras en lista o cantidad de nodos  */
	ql = 0;

	/* ptr al contenedor de direccion donde empieza la cadena */
	st_n1 = (stptr *) &st_p1;

	/* armo una cadena de nodos, cada nodo es una estructura 
	 * en st_n1 ... queda apuntando al ultimo nodo para agregar
	 */

	ql=0;
	for (j=0; j< qv; j++)
	{
		sprintf (b1,"e08_campo %03d",ql+101);

		*st_n1 = (stptr  ) malloc ( sizeof (nlst));
		(**st_n1).wrd = ( char *) malloc(32);
		sprintf ( (**st_n1).wrd,"%s",b1);
		(**st_n1).num = ql + 101;
		(**st_n1).nx = (stptr) NULL;
		st_n1 = (stptr *) & (*st_n1)->nx;

		ql++;

	}

	printf ("\n");

	
	/* vuelvo st_n1 a donde empieza la cadena */
	st_n1 = (stptr *) &st_p1;

	/* armo una cadena de nodos, cada nodo es una estructura 
	 * en st_n1 ... queda apuntando al ultimo nodo para agregar
	 */


	j=0;
	while ( (*st_n1) != (stptr ) NULL )
	{
		printf (sformat1,
			j, "(**st_n1).num", (**st_n1).num,
			   "(**st_n1).wrd", (**st_n1).wrd      );

		st_n1 = (stptr *) & (*st_n1)->nx;
		j++;
	}

	printf (" - - - - \n\n");




/*
 *
 *	9) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=9;
	printf ("9) Cadena de estructs - armo en funcion y tengo acceso en prog  \n");


	st_n1 = (stptr *) &st_p1;
	ff5(st_n1);

	j=0;
	st_n1 = (stptr *) &st_p1;

	while ( (*st_n1) != (stptr ) NULL )
	{
#if 0
		printf ("%3d  (**st_n1).num: %3d  (**st_n1).wrd: |%s|\n",j,(**st_n1).num,(**st_n1).wrd);
#endif

		printf (sformat1,
			j, "(**st_n1).num", (**st_n1).num,
			   "(**st_n1).wrd", (**st_n1).wrd      );

		st_n1 = (stptr *) & (*st_n1)->nx;
		j++;
	}

	printf (" - - - - \n\n");






/*
 *
 *	10) pasaje de variables a funciones 
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *	ff3
 *	recibe un puntero puntero a puntero de estructura
 *	imprime el contenido
 *
 *	ff4
 *	idem ff3, pero recibe dos arg iguales
 *	este formato seria el ideal a usar si
 *	queremos comparar contenido de dos cadenas
 *	de info (estructuras como lista de nodos encadenados)
 *	
 */

	i=10;
	printf ("10) Cadena de estructs - armo en funcion y tengo acceso en prog  \n");


	st_n2 = (stptr *) &st_p2;
	ff5(st_n2);

	j=0;
	st_n2 = (stptr *) &st_p2;

	while ( (*st_n2) != (stptr ) NULL )
	{
		printf (sformat1,
			j, "(**st_n2).num", (**st_n2).num,
			   "(**st_n2).wrd", (**st_n2).wrd      );

		st_n2 = (stptr *) & (*st_n2)->nx;
		j++;
	}

	printf (" - - - - \n\n");





/*
 *
 *	11) pasaje de variables a funciones 
 *	    similar al anterior, pero con 2 argumentos a funcion
 *	    seria el modo ideal de mandar dos cadenas de estructuras
 *	    a una funcion para que haga algo (comparar ? , merge ? )
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *
 *	ff6
 *	recibe dos punteros a punteros de estructuras
 *	arma una cadena sobre cada puntero
 *	
 */

	i=11;
	printf ("11) Cadena de estructs (2) - armo en funcion y tengo acceso en prog  \n");



	printf ("loop de armado de lista 8 \n");
	st_n1 = (stptr *) &st_p1;
	st_n2 = (stptr *) &st_p2;
	ff6(st_n1,st_n2);

	j=0;
	st_n1 = (stptr *) &st_p1;
	st_n2 = (stptr *) &st_p2;

	/* para simplificar, ambas cadenas tienen la misma cantidad de nodos */
	while ( (*st_n1) != (stptr ) NULL && (*st_n2) != (stptr ) NULL )
	{
		sprintf (buf,"%03d",j);

		printf (sformat3,
			buf, 
			"(**st_n1).num", (**st_n1).num, "(**st_n1).wrd", (**st_n1).wrd,
			"(**st_n2).num", (**st_n2).num, "(**st_n2).wrd", (**st_n2).wrd
			);

		/* avanzo al proximo nodo (para ambas listas ) */
		st_n1 = (stptr *) & (*st_n1)->nx;
		st_n2 = (stptr *) & (*st_n2)->nx;
		j++;
	}

	printf (" - - - - \n\n");



/*
 *
 *	12) pasaje de variables a funciones 
 *	    similar al anterior, pero con 2 argumentos a funcion
 *	    seria el modo ideal de mandar dos cadenas de estructuras
 *	    a una funcion para que haga algo (comparar ? , merge ? )
 *	
 *	paso puntero a puntero a puntero del primer nodo,
 *	cuando vuelvo de la rutina, sigo teniendo acceso
 *	a toda la cadena y al ultimo nodo.
 *	
 *
 *	usamos un vector de punteros a estructura
 *	inicializamos el puntero a puntero
 *	con la direccion del puntero anterior
 *	imprimimos contenido
 *
 *
 *	ff7
 *	recibe dos punteros a punteros de estructuras
 *	arma una cadena sobre cada puntero
 *	
 */

	i=12;
	printf ("12) Cadena de estructs (2) - armo en funcion y tengo acceso en prog  \n");


	st_nx = &st_n1;
	st_ny = &st_n2;

	/* guardo la direccion del primer nodo. en st_n1 va a quedar la direccion del ultimo desp */
	st_n2 = st_n1;

	/* creo dos listas de nodos encadenados */
	ff7(st_nx,st_ny);

	j=0;
	while ( (*st_n2) != (stptr ) NULL )
	{
		printf (sformat1,
			j, 
			"(**st_n2).num", (**st_n2).num, "(**st_n2).wrd", (**st_n2).wrd  );

		st_n2 = (stptr *) & (*st_n2)->nx;
		j++;
	}

	printf (" - - - - \n\n");


/*
 *
 *	13) pasaje de variables a funciones 
 *	    Para un puntero a un vector de estructuras a funcion,
 *	    para que inicialice.
 *	    Es lo que usariamos para cargar varios archivos
 *	    diferentes, a un mismo tipo de estructura,
 *	    usando la misma funcion.
 *
 *
 *	
 */

	i=13;
	printf ("13) Inicializo un vector de punteros a estructs en una funcion   \n");

	st_n1 = &v1[0];
	st_nx = &st_n1;

	/* imprimo el estado actual */
	ff3(st_nx);

	/* mando a inicializar */
	ff8(st_nx);
	
	/* imprimo el nuevo estado  */
	ff3(st_nx);

	
	printf (" - - - - \n\n");


/*
 *
 *	14) pasaje de variables a funciones 
 *	    Para un puntero a un vector de estructuras a funcion,
 *	    para que inicialice.
 *	    Es lo que usariamos para cargar varios archivos
 *	    diferentes, a un mismo tipo de estructura,
 *	    usando la misma funcion.
 *
 *	idem anterior, pero con un nivel menos de indireccion
 *
 *	
 */

	i=14;
	printf ("14) Inicializo un vector de punteros a estructs en una funcion   \n");

	st_n1 = &v1[0];

	/* imprime estado */
	ff9(st_n1);

	/* inicializo */
	ffa(st_n1);

	/* imprime estado */
	ff9(st_n1);

	printf (" - - - - \n\n");




	/* prueba */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale proceso prueba 1 \n\n",gp_tm());
	}

	printf ("\n");
}





/*
 *	ff1
 *	recibe un puntero a estructura
 *	imprime contenido
 *
 */

int	ff1(p1)
stptr	p1;
{
	printf (sformat2,
		"ff1", "(*p1).num", (*p1).num,
		       "(*p1).wrd", (*p1).wrd        );
}

/*
 *	ff2
 *	recibe un puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ff2(q1)
stptr	*q1;
{
	printf (sformat2,
		"ff2", "(**q1).num", (**q1).num,
		       "(**q1).wrd", (**q1).wrd        );

}

/*
 *	ff3
 *	recibe un puntero a puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ff3(r1)
stptr	**r1;
{
	int 	i,j;

	for (i=0; i< qv; i++)
	{
		printf (sformat2,
			"ff3", "(*(*r1)[i]).num", (*(*r1)[i]).num ,
			       "(*(*r1)[i]).wrd", (*(*r1)[i]).wrd     );
	}
}

/*
 *	ff4
 *	recibe un puntero a puntero a puntero de estructura
 *	imprime contenido
 *	dos argumentos iguales 
 */

int	ff4(r1,r2)
stptr	**r1,**r2;
{
	int 	i,j;

	for (i=0; i< qv; i++)
	{
		printf (sformat3,
			"ff4", 
			"(*(*r1)[i]).num", (*(*r1)[i]).num, "(*(*r1)[i]).wrd", (*(*r1)[i]).wrd,
			"(*(*r2)[i]).num", (*(*r2)[i]).num, "(*(*r2)[i]).wrd", (*(*r2)[i]).wrd
			);
	}

}

/*
 *	ff5
 *	recibe un puntero a puntero de estructura
 *	arma una lista encadenada de elementos
 *	
 *	atencion: ojo con esto !
 *	al terminar ... 
 *	p1 apunta al ultimo eslabon de la cadena
 *	pero al volver de la rutina, ese valor
 *	se pierde !
 *	El llamador solo tiene la direccion del
 *	primer eslabon de la cadena ...
 *	Tendria que recorer toda la cadena para 
 *	poder volver a agregar eslabones.
 */

int	ff5(p1)
stptr	*p1;
{

	int	ql,i;
	char	b1[MAXB];

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff5_campo %03d",ql+201);

		*p1 = (stptr  ) malloc ( sizeof (nlst));
		(**p1).wrd = ( char *) malloc(20);
		sprintf ( (**p1).wrd,"%s",b1);
		(**p1).num = ql + 201;
		(**p1).nx = (stptr) NULL;
		p1 = (stptr *) & (*p1)->nx;

		ql++;

	}

}


/*
 *	ff6
 *	idem ff5 pero con dos argumentos iguales
 *
 *	recibe un puntero a puntero de estructura
 *	arma una lista encadenada de elementos
 *	
 *	atencion: ojo con esto !
 *	al terminar ... 
 *	p1 apunta al ultimo eslabon de la cadena
 *	pero al volver de la rutina, ese valor
 *	se pierde !
 *	El llamador solo tiene la direccion del
 *	primer eslabon de la cadena ...
 *	Tendria que recorer toda la cadena para 
 *	poder volver a agregar eslabones.
 */


int	ff6(p1,p2)
stptr	*p1,*p2;
{
	int	ql,i;
	char	b1[MAXB];

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff6_campo %03d",ql+301);

		*p1 = (stptr  ) malloc ( sizeof (nlst));
		(**p1).wrd = ( char *) malloc(20);
		sprintf ( (**p1).wrd,"%s",b1);
		(**p1).num = ql + 301;
		(**p1).nx = (stptr) NULL;
		p1 = (stptr *) & (*p1)->nx;

		ql++;

	}

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff6_campo %03d",ql+401);

		*p2 = (stptr  ) malloc ( sizeof (nlst));
		(**p2).wrd = ( char *) malloc(20);
		sprintf ( (**p2).wrd,"%s",b1);
		(**p2).num = ql + 401;
		(**p2).nx = (stptr) NULL;
		p2 = (stptr *) & (*p2)->nx;

		ql++;

	}

}

/*
 *	ff7
 *	recibe dos argumentos iguales
 *
 *	recibe puntero a puntero puntero de estructura
 *	arma una lista encadenada de elementos
 *	
 *	atencion: ojo con esto !
 *	(la diferencia con ff6, un nivel mas de indireccion)
 *
 *	al terminar ... 
 *	r1 apunta al puntero al ultimo eslabon de la cadena
 *	de manera que al volver de la rutina,
 *	ese valor le queda al caller porque
 *	r1 apunta a un valor que lo tiene registrado
 */


int	ff7(r1,r2)
stptr	**r1,**r2;
{
	int	ql,i;
	char	b1[MAXB];

	ql=0;
	for (i=0; i< qv; i++)
	{
		sprintf (b1,"ff7_campo %03d",ql+201);

		**r1 = (stptr  ) malloc ( sizeof (nlst));

		(***r1).wrd = ( char *) malloc(20);
		sprintf ( (***r1).wrd,"%s",b1);
		(***r1).num = ql + 201;
		(***r1).nx = (stptr) NULL;
		*r1 = (stptr *) & (**r1)->nx;
		ql++;
	}

}



/*
 *	ff8
 *	recibe un puntero a puntero a puntero de estructura
 *	inicializa contenido
 *
 */

int	ff8(r1)
stptr	**r1;
{
	int 	i,j;

	for (i=0; i< qv; i++)
	{
		sprintf ( (*(*r1)[i]).wrd, "ff8_campo %03d", i);
		(*(*r1)[i]).num = i;
	}
}


/*
 *	ff9
 *	recibe un puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ff9(q1)
stptr	*q1;
{
	int	i;

	for (i=0; i< qv; i++)
	{
		printf (sformat2,
			"ff9", "(*q1[i]).num", (*q1[i]).num,
			       "(*q1[i]).wrd", (*q1[i]).wrd        );
	}
}


/*
 *	ffa
 *	recibe un puntero a puntero de estructura
 *	imprime contenido
 *
 */

int	ffa(q1)
stptr	*q1;
{
	int	i;

	for (i=0; i< qv; i++)
	{
		sprintf ( (*q1[i]).wrd, "ffa_campo %03d",i);
		(*q1[i]).num = i;
	}
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prueba 2
 *
 *	pruebas ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue2
 *
 *	abre archivo con lista de archivos a procesar (fuentes fortran)
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *
 *	carga todos los archivos en el vector de memoria,
 *	o sea, TODOS los fuentes, estan en memoria
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_prue2()
{

	int	i,j,k;
	char	d1[MAXB];
	char	d2[MAXB];
	int	ql,qlf;
	int	flag;
	int	q_ptr;

	FILE	*hwi,*hwo;


	char	z[MAXV];
	sprintf (z,"prue2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffdat )
		gp_uso(106);



	/* cantidad de lineas en el archivo  */
	qf_ff = 0;
	q_ptr = 0;
	ql=0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
			ql++;
		}
	}


	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de lineas cargadas:  %5d \n",ql);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/* proceso todos los files */
#if 0
	p_src1();
	p_src2();
#endif
	p_src3();



	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(608);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);


		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	p_src3         
 *
 *	busca patrones de la forma   var = / xxxxxx /
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	busca patrones de inicializacion vieja de variables
 *	de la forma var = / xxxx /
 *
 */


int	p_src3()
{

	int	i,j,k,k1,l;
	int	f1,f2,f3,f4,f5;
	int	pf,uf;
	int	m1,m2;
	int	l1;
	int	p1;
	int	fl_n;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];

	char	d1[MAXR];
	char	d2[MAXR];


	memset (b3,'X',MAXB);

	/* por cada uno de los fuentes cargados */
	for (fl_n=0; fl_n< qf_ff; fl_n++)
	{
		if (gp_fverbose("d1"))
		{
			printf ("proceso: %6d %6d |%s|\n",
				(*tb[fl_n]).pf,(*tb[fl_n]).uf,(*tb[fl_n]).n);

		}


		/* primera y ultima fila del source */
		pf = (*tb[fl_n]).pf;
		uf = (*tb[fl_n]).uf;

		/* reviso cada linea */
		for (j = pf; j <= uf; j++)
		{
			/* la linea j */
			strcpy (b1, (*fnp[ j ]).l );
			strcpy (b0, (*fnp[ j ]).l );

			/* parseo fila a tokens ... si tiene sentido  */
			l_pars(j,&q_tk);


			/*
			 * detectar si la linea es comentarios ...
			 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
			 */

			f4 = 1;
			if (es_linea_comentario(b1))
				f4 = 0;
	
			/* solo proceso lineas que no son comentario */
			if (f4)
			{	
				f5 = 1;
				while ( f5 )
				{

					strcpy(b2,b1);

					f5 = 0;
					if (p1 = es_cadena_int_src3(b1,&m1))
					{	f5 = 1;

						memset (d1,0,MAXR);
						strncpy (d1,b1+p1,m1);
		printf ("YYY: |%s| \n",d1);
						memset (d2,0,MAXR);
						strcpy(d2,d1);
						d2[0] = '=';
						d2[m1-1] = ' ';
		printf ("YYY: |%s| \n",d2);


						strncpy(b1+p1,b3,m1);
						strncpy(b1+p1,d2,m1);

						sprintf (b4,"f:%-30.30s l:%06d |%s| \n", (*tb[fl_n]).n,j,b2 );
						sprintf (b5,"f:%-30.30s l:%06d |%s| \n", (*tb[fl_n]).n,j,b1 );
						
						if (gp_fverbose("d3"))
						{
							printf ("TTX1\n");
							printf ("TTX2%s",b4);
							printf ("TTX3%s",b5);
							printf ("TTX4\n");
						}

						if (ffout)
						{
							fprintf (hfout,"%s",b4);
							fprintf (hfout,"%s",b5);
							fprintf (hfout,"%s","\n");
						}

						if (fflog)
						{
							fprintf (hflog,"%s",b4);
							fprintf (hflog,"%s",b5);
							fprintf (hflog,"%s","\n");
						}

					}
				}

			}	


			/* grabo la linea */
			if ( 1 )
			{
#if 1
				/* cuando terminos pruebas, va este */
				strcpy ((*fnp[ j ]).l, b1 );
#endif

#if 0
				strcpy ((*fnp[ j ]).l, b0 );
#endif
			}
		}
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	es_cadena_int_src3  
 *
 *	es cadena interesante para sub p_src3  - busca var = / xxxx /
 *
 * -----------------------------------------------------------------------------------
 */


int	es_cadena_int_src3(s,largo)
char	*s;
int 	*largo;
{
	char	c,d;
	int	i,j,k;
	int	l1;
	int	p1,p2,p3;
	int	f1,f2,f3,f5;
	int	m1;


	/* ej. log  variable=/.false./    */
	/* ej. char variable=/'X'/        */


	m1 = 0;
	f5 = 0;
	l1 = 0;
	p1 = 0;
	
	for (i=0, f1 = 1, f2 = 0;  f1 && s[i]; i++)
	{
		c = s[i];
		d = s[i+1];

		if ( !f2 && i > 10 && c == '!')
			f1 = 0;

		if ( !f2 && c == '/' && d == '/')
			f1 = 0;

		if ( !f2 && c == '/' )
			f2 = 1, p1 = i;

		if ( f2 && l1 && c != '/' )
		{
			f3 = 0;

			/* caracteres que aceptamos como parte de var=/xxxxx/ */
			if ( c >= 'A' && c <= 'Z' ) 
				f3 = 1;
			if ( c >= 'a' && c <= 'z' ) 
				f3 = 1;
			if ( c >= '0' && c <= '9' ) 
				f3 = 1;
			if ( c == '_' || c == '.' )
				f3 = 1;
			if ( c == '\'' )
				f3 = 1;
			if ( c == '-' )
				f3 = 1;
			if ( c == ' ' )
				f3 = 1;

			if (!f3)
				f1 = 0;
		}

		if ( f2 && l1 && c == '/' )
		{
			l1++;
			f1 = 0;
			f2 = 0;
			f5 = p1;
		}

		if ( f2 )
			l1++;
	}	
			
#if 0
	printf ("TTT5:es_cadena_int_src3:          l1:%2d f5:%2d |%s| \n",l1,f5,s);
#endif

	*largo = l1;
	return (f5);
}



#if 0

/*
 *	es_cadena_interesante
 *
 *	busca en un string (linea de source)
 *	inicializacion de variables formato antiguo  
 *	var = / xxxx /
 *	
 */

int	es_cadena_int_src3(s)
char	*s;
{
	char	c;
	int	i,j,k;
	int	l1;
	int	p1,p2,p3;
	int	f1,f2,f3;
	int	f9;
	int	target;
	int	s_count;
	char	b1[MAXB];
	char	b2[MAXB];



	target = 10;	/* miremos cadenasde mas de 10 caracteres por ahora */
	f9 = 0;		/* cadena interesante 0 no 1 si */

	strcpy(b1,s);


	s_count = 0;

	p1 = 0;
	f1 = 1;
	f2 = 1;
	f3 = 1;
	l1 = 0;
	i  = 0;

printf ("es_cadena: |%s| \n",s);

	while ( f2 )
	{
		c = b1[p1 + i];
		k = 0;
			
		if ( c >= 'a' && c <= 'z' )
			k = 1;

		if ( c >= 'A' && c <= 'Z' )
			k = 1;

		if ( c >= '0' && c <= '9' )
			k = 2;

		if ( c == ' ' )
			k = 3;

		if ( c == '_' )
			k = 4;

		if ( c == '.' )
			k = 5;

		if ( c == '/' )
			k = 6;

printf ("while: i: %d f9: %d k/c: %c%c |%s| \n",i,f9,c,b1[1+i+1],s);

		switch (k)
		{
			case 	6:
				if ( !f9 && i == 0 )
					f9 = 1, i++;

				if ( !f9 && i )
					i++;

				if ( f9 && i > 0 )
					f2 = 0;

				break;

			default:
				if ( c == 0 )
					f2 = 0;
				i++;
				break;
					

#if 0
				/* no es caracter valido en la cadena, terminamos */
			case	0:
				f2 = 0;
				if (f1 == 0)
					f9 = i;
				else
					f9 = 0;
				break;

				/* es letra */
			case	1:
				if (f1)
					f1=0;
				i++;
				break;

				/* es numero , solo si hubo una letra antes */
			case	2:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;


				/* aceptamos casos en los que hay blancos como parte del nombre !! */
			case 	3:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;

				/* aceptamos casos en los que hay _ como parte del nombre !! */
			case 	4:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;
#endif
		}			
	}



	return (f9);

}



#endif




/* EEE */

/*
 * -----------------------------------------------------------------------------------
 *
 *	quitar_blancos_molestos         
 *
 *	elimina blancos al final de una linea
 *
 * -----------------------------------------------------------------------------------
 */

char	*quitar_blancos_molestos(s)
char	*s;
{

	static	char	m0[MSTR];
	int	p1,p2;
	int	l1,l2;
	int	i,k;
	int	f_sigo;
	int	f1;

	char	z[MAXV];
	int	w;
	sprintf (z,"quitar_blancos_molestos");
	w = g_dbg(z);


	strcpy(m0,s);
	l1 = strlen(m0);
	
	p1    = 0;
	p2    = l1;
	



	f_sigo = 1;
	f1     = 0;

	/* tengo que saltear la palabra allocate y llegar al ( 
	 * tengo que buscar allocate para contemplar
	 * sentencias que tienen if ... allocate 
	 */

if (gp_debug && w)
{
mprintf (z,"voy a buscar el allocate \n");
}
	while (f_sigo)
	{
		p1++;

if (gp_debug && w)
{
mprintf (z,"p1: %d\n",p1);
mprintf (z,"st: |%s|\n",m0+p1);
}

		if ( !strncmp(m0+p1,"llocate",7) || !strncmp(m0+p1,"LLOCATE",7) )
		{
			f1  = 1;
			p1 += 7; 
			f_sigo = 0;
		}

#if 0
		if (f1)
		{
			if (m0[p1] == '(')
				f_sigo = 0;
		}
#endif

#if 0
		if (p1 > 2000 )
			error(4001);
#endif
	}


	f_sigo = 1;

if (gp_debug && w)
{
mprintf (z,"voy a sacar blancos \n");
}

	/* saco los blancos intermedios que molestan */
	while (f_sigo)
	{
 		if (m0[p1] == ' ')
		{
			l2 = strlen(m0)-1;
			for (k=p1; k<l2; k++)
				m0[k] = m0[k+1];
			m0[l2]=0;
			p2--;
		}

		if (m0[p1] == '!')
			f_sigo = 0;

		if (m0[p1] != ' ')
			p1++;

		l1 = strlen(m0);
		if (p1 >= l1 -1 )
			f_sigo = 0;

	}

if (gp_debug && w)
{
mprintf (z,"s : |%s|\n",s);
mprintf (z,"m0: |%s|\n",m0);
}

	return  m0;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	trim_blanks         
 *
 *	elimina blancos al final de una linea
 *
 * -----------------------------------------------------------------------------------
 */



char	*trim_blanks(s)
char	*s;
{
	static	char	b[MAXB];
	int	i;
	int	f4;
	
	strcpy(b,s);

	/* blancos al final */
	for (i=strlen(b)-1, f4=1 ; i && f4 ; i-- )
		if (b[i] == ' '  )
			b[i]=0;
		else
			f4=0;

	return (b);
}


 

/*
 * -----------------------------------------------------------------------------------
 *
 *	trim_blanks_beg         
 *
 *	elimina blancos al final de una linea
 *
 * -----------------------------------------------------------------------------------
 */


char	*trim_blanks_beg(s)
char	*s;
{
	static	char	b[MSTR];
	int	i;
	int	f4;
	int	l2;
	int	f_sigo;
	int	p1;
	int	offset;


	strcpy(b,s);
	l2 = strlen(b);
	offset=0;

	/* blancos al principio */
	for (i=0, p1=0, f_sigo=1; f_sigo && i<l2; i++ )
	{	if (b[i] == ' ' || b[i] == '\t'  )
		{	p1=i;
			offset = 1;
		}
		else
			f_sigo = 0;
	}
	
	return (b+p1+offset);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	n_blanks_beg         
 *
 *	elimina blancos al final de una linea
 *
 * -----------------------------------------------------------------------------------
 */


int	n_blanks_beg(s)
char	*s;
{
	static	char	b[MSTR];
	int	i;
	int	f4;
	int	l2;
	int	f_sigo;
	int	p1;
	int	offset;


	strcpy(b,s);
	l2 = strlen(b);
	offset=0;

	/* blancos al principio */
	for (i=0, p1=0, f_sigo=1; f_sigo && i<l2; i++ )
	{	if (b[i] == ' ' || b[i] == '\t'  )
		{	p1=i;
			offset = 1;
		}
		else
			f_sigo = 0;
	}
	
	return (p1+offset);
}


 
/*
 * -----------------------------------------------------------------------------------
 *
 *	pasa_filtro         
 *
 *	verifica si es palabra reservada del lenguaje
 *
 * -----------------------------------------------------------------------------------
 */



int	pasa_filtro(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];

	int	target;


	target = 20;	/* por ahora dejasmos este largo ... */


	strcpy(b1,s);
	f1 = 0;
	f2 = 1;


	if (f2 && !strncmp("real",pasar_a_minusc(b1),4))
		f2 = 1, f1 = 31;
	
	if (f2 && !strncmp("integer",pasar_a_minusc(b1),7))
		f2 = 1, f1 = 32;
	
	if (f2 && !strncmp("logical",pasar_a_minusc(b1),7))
		f2 = 1, f1 = 33;
	
	if (f2 && !strncmp("character",pasar_a_minusc(b1),9))
		f2 = 1, f1 = 34;
	
	if (f2 && !strncmp("if",pasar_a_minusc(b1),2))
		f2 = 1, f1 = 35;
	
	if (f2 && !strncmp("use",pasar_a_minusc(b1),3))
		f2 = 1, f1 = 21;
	
	if (f2 && !strncmp("call",pasar_a_minusc(b1),4))
		f2 = 1, f1 = 22;
	
	if (f2 && !strncmp("subroutine",pasar_a_minusc(b1),10))
		f2 = 1, f1 = 23;

	if (f2 && !strncmp("include",pasar_a_minusc(b1),7))
		f2 = 1, f1 = 24;
	



	return (f1);

}




/*
 *	es_cadena_interesante
 *
 *	busca en un string (linea de source)
 *	nombres 'largos' (pongamos, mayor a 30 chars )
 *	
 */

int	es_cadena_interesante(s)
char	*s;
{
	int	i,j,k;
	int	l1;
	int	p1,p2,p3;
	int	f1,f2,f3;
	int	f9;
	int	target;
	char	c;
	char	b1[MAXB];
	char	b2[MAXB];



	target = 10;	/* miremos cadenasde mas de 10 caracteres por ahora */
	f9 = 1;		/* cadena interesante 0 no 1 si */

	strcpy(b1,s);

#if 1


	p1 = 0;
	f1 = 1;
	f2 = 1;
	f3 = 1;
	l1 = 0;
	i  = 0;


	while ( f2 )
	{
		c = b1[p1 + i];
		k = 0;
			
		if ( c >= 'a' && c <= 'z' )
			k = 1;

		if ( c >= 'A' && c <= 'Z' )
			k = 1;

		if ( c >= '0' && c <= '9' )
			k = 2;

		if ( c == ' ' )
			k = 3;

		if ( c == '_' )
			k = 4;

		switch (k)
		{
				/* no es caracter valido en la cadena, terminamos */
			case	0:
				f2 = 0;
				if (f1 == 0)
					f9 = i;
				else
					f9 = 0;
				break;

				/* es letra */
			case	1:
				if (f1)
					f1=0;
				i++;
				break;

				/* es numero , solo si hubo una letra antes */
			case	2:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;


				/* aceptamos casos en los que hay blancos como parte del nombre !! */
			case 	3:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;

				/* aceptamos casos en los que hay _ como parte del nombre !! */
			case 	4:
				if (f1)
				{
					f9 = 0;
					f2 = 0;
				}
				else
					i++;
				break;
		}			
	}


#endif



	return (f9);

}




/*
 *	una forma de encontrar long names ...
 *
 */


int	p_src1()
{

	int	i,j,k,l;
	int	f1,f2,f3,f4;
	int	pf,uf;

	char	b1[MAXB];


	printf ("proceso file ... \n");

	for (i=0; i< qf_ff; i++)
	{
		if (gp_fverbose("d1"))
		{
			printf ("proceso: %6d %6d |%s|\n",
				(*tb[i]).pf,(*tb[i]).uf,(*tb[i]).n);

		}


		pf = (*tb[i]).pf;
		uf = (*tb[i]).uf;

		for (j = pf; j <= uf; j++)
		{
			/* parseo fila a tokens */
			l_pars(j,&q_tk);


			/*
			 * detectar si la linea es comentarios ...
			 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
			 */

			f4 = 1;
				if (!strcmp("!",tk[0]))
					f4 = 0;
				if (!strcmp("C",tk[0]))
					f4 = 0;
				if (!strcmp("c",tk[0]))
					f4 = 0;

			for (k=0; f4 && k<q_tk; k++)
			{


				if (es_cadena_valida(1,tk[k]))
				{
					memset (b1,0,MAXB);
					f1 = 1;
					f2 = 1;

					while (f1)
					{
						strcat(b1,tk[k]);
						
						k++;
						if (k == q_tk)
							f1 = 0;

						if (f1 && !es_cadena_valida(2,tk[k]) )
							f1 = 0, f2 = 0;
					}

					/*
					 * ojo ... falta hacer un trim si hay blancos al final 
					 *
					 */
					if (f2)
					{
						if (strlen(b1) > 25)
							fprintf (hfout,"|%s|\n",b1);
					}
				} 
			} 
		}
	}
}




int	es_cadena_valida(pos,s)
int	pos;
char	*s;
{
	int	i,j,k,flag;
	int	f1,f2;
	char	c;

	flag=1;
	if (!strcmp("use",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("subroutine",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("entry",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("call",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("end",pasar_a_minusc(s) ))
		flag = 0;
	if (!strcmp("integer",pasar_a_minusc(s) ))
		flag = 0;

	for (i=0; flag && i<strlen(s); i++)
	{
		c = s[i];

		flag = 0;

		if (pos == 1)
		{	
			if ( c >= 'a' && c <= 'z' )
				flag = 1;
			if ( c >= 'A' && c <= 'Z' ) 
				flag = 1;

		}

		if (pos == 2)
		{
			if ( c >= 'a' && c <= 'z' )
				flag = 1;
			if ( c >= 'A' && c <= 'Z' ) 
				flag = 1;
			if ( c >= '0' && c <= '9' )
				flag = 1;
			if ( c == ' ' || c == '_' )
				flag = 1;
		}

	}

	return flag;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 3
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue3
 *
 *	Descripcion:
 *
 */


/* bloque */
#if 1


int	pro_prue3()
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;

	char	*v1[100];
	char	**s1;
	char	t[100][64];

	char	z[MAXV];
	sprintf (z,"prue3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

#if 0
	if (!1 || !2 )
		gp_uso(107);
#endif

	/* bloque */
	for (i=0; i<100; i++)
	{
		sprintf (t[i],"Param %3d",i);
		v1[i] = t[i];
	}

	s1 = &v1[0];

	for (i=0; i<100; i++)
	{
		printf ("Param : %3d  = |%s| \n",i,*(s1+i) );
	}	


		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */





/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	prue4
 *
 *	hago un malloc de un espacio y lo convierto en vector de punteros 
 *	a estructuras.
 */


#if 0
typedef	struct	tfn	*fnptr;
typedef	struct	tfn
{	char	l[HUG2];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	node;
#endif


/* bloque */
#if 1

int	pro_prue4()
{
	int	i,j,k;
	int	f1,f2,f3;
	int	x1,x2;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;

	fnptr	*pp;

	char	z[MAXV];
	sprintf (z,"prue4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!1 || !2 )
		gp_uso(108);


	x1 = 200;

	pp = (void *) malloc( x1 * sizeof (fnptr) );
	if ( pp == NULL )
		error(333);
	printf ("malloc ok !! \n");



	for (i=0; i<x1; i++)
	{
		pp[i] = (fnptr ) malloc ( sizeof (node) );
		if ( pp[i] == NULL )
			error(334);
	}

	for (i=0; i<x1; i++)
	{
		(*pp[i]).f1 = 1000 + i;
		(*pp[i]).f2 = 2000 + i;
	}
 
	for (i=0; i<x1; i++)
	{
		printf ("%3d %5d %5d \n",i,(*pp[i]).f1 , (*pp[i]).f2 );
	}
 
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 5
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	prue5
 *
 *	carga un fuente fortran entero sobre estuctura de cadena de estrcts 
 *	si queremos cargar toda la base en memoria, y queremos extender 
 *	un programa, tendriamos que estar moviendo 500,000 lineas a cada rato !
 */

/*
 *	2018-04-21 
 *
 *	bd_load
 *	carga la base de datos del diccionario
 *	diccio base: diccio01.txt
 *
 */



/* bloque */
#if 1

int	pro_prue5()
{
	int	flag;
	int	ql;

	int	i,j,k;
	int	f1,f2,f3;
	int	x1,x2;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;


	char	z[MAXV];
	sprintf (z,"prue5");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!1 || !2 )
		gp_uso(109);

#if 1



	/* cantidad de palabras en el diccionario */
	q_wrd = 0;

	/* ptr al contenedor de direccion del ultimo nodo de la cadena */
	npa = (pnodeptr *) &np1;

	ql=0;
	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(b1) )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			if (gp_fverbose("d3"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}

			*npa = (pnodeptr ) malloc ( sizeof (knode));
			(**npa).wrd = ( char *) malloc(strlen(b1)+1);
			sprintf ( (**npa).wrd,"%s",b1);
			(**npa).num = q_wrd+1;
			(**npa).nx = (pnodeptr) NULL;
			npa = (pnodeptr *) & (*npa)->nx;

			q_wrd++;
			ql++;
		}
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de palabras en diccionario  : %6d\n",q_wrd);
	}


	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad del diccionario : \n\n");
	
		i=0;
		npa = (pnodeptr *) &np1;

		while ( (*npa) != (pnodeptr) NULL )
		{
			printf ("i: %3d  (**npa).num: %3d  (**npa).wrd: |%s|\n",i,(**npa).num,(**npa).wrd);
			npa = (pnodeptr *) & (*npa)->nx;
			i++;
		}

		printf ("\n");
	}

#endif

		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */







/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 6
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	exṕerimentos para ver si puedo determinar en lineas comentadas
 *	que es un comentario humano, o que es una linea de codigo comentada
 *
 */

int	q_wrd;
char	cwd[10000][32];

int	q_cid;
char	cid[20000][MAXR];

#define	MAX_L	76

/* bloque */
#if 1

int	pro_prue6()
{
	int	flag;
	int	ql;

	int	i,j,k;
	int	c1,c2,c3;
	int	f1,f2,f3,f4;
	int	f_sig;
	int	x1,x2;
	char	c;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;


	char	z[MAXV];
	sprintf (z,"prue6");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffin2 || !ffou2 || !fflog  )
		gp_uso(110);



	/*
	 * cargo todos los comentarios que extraje
	 *
	 */

	ql=0;
	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		if (strlen(b1) > MAXB)
			error(2001);

		/* saco el fin de linea - contemplo 13 x fuentes fortran */
		for ( flag=0, j=strlen(b1); !flag && j; j--)
			if (b1[j] == '\n' )
			{	
				flag=1;

				if ( j && b1[j-1] == 13)
					b1[j-1]=0;
				else
					b1[j]=0;
			}

		strcpy(cpt[ql],b1);
		ql++;
	}

	/*
	 * cargo todos los token que extraje de comentarios 
	 *
	 */

	q_cid = 0;
	while (fgets(b1,MAXB,hfin2) != NULL)
	{
		if (strlen(b1) > MAX_L)
		{	printf ("%3d |%s|\n",strlen(b1),b1);
			error(2002);
		}

		/* saco el fin de linea - contemplo 13 x fuentes fortran */
		for ( flag=0, j=strlen(b1); !flag && j; j--)
			if (b1[j] == '\n' )
			{	
				flag=1;

				if ( j && b1[j-1] == 13)
					b1[j-1]=0;
				else
					b1[j]=0;
			}

		strcpy(cid[q_cid],b1);
		q_cid++;
	}


	/*
	 * cargo todas las palabras que suenan en comentarios de personas
	 *
	 */

	q_wrd = 0;
#if 1

	if ( 1 && ((hwi = fopen ("diccio_dep.txt","r")) == NULL) )
	{
		error(3001);
	}

	q_wrd = 0;
	while (fgets(b1,MAXB,hwi) != NULL)
	{
		if (strlen(b1) > 32)
		{	printf ("%3d |%s|\n",strlen(b1),b1);
			error(3002);
		}

		/* saco el fin de linea - contemplo 13 x fuentes fortran */
		for ( flag=0, j=strlen(b1); !flag && j; j--)
			if (b1[j] == '\n' )
			{	
				flag=1;

				if ( j && b1[j-1] == 13)
					b1[j-1]=0;
				else
					b1[j]=0;
			}

		strcpy(cwd[q_wrd],b1);
		q_wrd++;
	}

	fclose(hwi);

#endif

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de palabras en diccionario  : %6d\n",ql);
		printf ("Cantidad de terminos en index        : %6d\n",q_cid);
		printf ("Cantidad de palabras en dic dep      : %6d\n",q_wrd);
	}


	/*
	 * ciclos
	 * - marcar en index los terminos que parece que determinan una linea de codigo
	 * - utilizar esos terminos en la busqueda de comentarios
	 * - grabar en log para ver resultados
	 */

	/* cosas que parezcan variables, probablemente sean lineas comentadas */

	/*
	 * x1 
	 *
	 */

	for (i=0; i< q_cid; i++)
	{
		strcpy(b1,cid[i]+2);

		f2 = 1;

		/* el primer caracter tiene que ser una letra */
		if (f2)
		{
			c = b1[0];
			if ( !( (c >= 'A' && c <= 'Z' ) || (c >= 'a' && c<= 'z') )  )
				f2 = 0;
		}

		/* tiene que haber solo combinacion de letras / numeros / _ */
		if (f2)
		{
			for (j=0, f1=1, f2=1; f1 && j< strlen(b1); j++)
			{
				c = b1[j];
	
				if ( !((c >= 'A' && c <= 'Z' ) || (c >= 'a' && c<= 'z') || ( c == '_') || ( c >= '0' && c <= '9'))  )
					f1 = 0, f2 = 0;
			}
		}

		/* el caracter _ tiene que estar al menos 1 vez */
		if (f2)
		{
			for (j=0, c1 = 0, f1=1, f2=1; f1 && j< strlen(b1); j++)
			{
				c = b1[j];
	
				if ( c == '_') 
					c1++;
			}

			if ( c1 == 0 )
				f2 = 0;

		}

		/* las cadenas tipo cccccccccc no van */
		if (f2)
		{
			for (j=0, c1 = 0, c2 = 0, f1=1, f2=1; f1 && j< strlen(b1); j++)
			{
				c = b1[j];
	
				if ( c == 'c') 
					c1++;
				if ( c == 'C') 
					c2++;
			}

			if ( c1 > 8 )
				f2 = 0;
			if ( c2 > 8 )
				f2 = 0;
		}



		/* si paso todas las condiciones, entonces puede ser un termino de interes */
		if (f2)
		{
			cid[i][0] = '1';
		}
	}


	/*
	 * testeo que comentarios surgen al chequear terminos de interes
	 *
	 */

	for (i=0; i < q_cid; i++)
	{	
		f3 = 0;
		if (cid[i][0] == '1')
			f3 = 1;

		if ( f3 )
		{
			strcpy(b1,cid[i]+2);

			if (gp_fverbose("d4"))
			{
				printf ("Probando termino : |%s| \n",b1);
			}

			for (j=0; j < ql; j++)
			{
				strcpy(b2,cpt[j]+2);


				for (k=0, f_sig = 1, f4 = 0; f_sig && k < strlen(b2); k++)
				{
					if (!strcmp(b1,b2+k))
						f_sig = 0, f4 = 1;
				}


				/* 
				 * En este punto ...
				 * Tengo:
				 * - indentificado un termino (con todas las condiciones para que sea un termino considerado valido)
				 * - encuentro 1 comentario que clasifica como "linea de codigo comentada" x el termino
				 * 
				 * Pero ... ahora debo aplicar todas las condiciones que anulan la clasificacion valida,
				 *          pero ya sin aplicar mas conficiones al termino ....
				 */

				if (f4)
				{
					/* marco este comentario como 'clasificado linea de codigo' */
					cpt[j][0] = '1';
					fprintf (hflog,"%-48.48s | %s\n",b1,b2);

#if 0
					fprintf (hflog,"|%s|\n",b1);
					fprintf (hflog,"|%s|\n",b2);
					fprintf (hflog,"|%s\n"," ");
#endif
				}
			}
		}
	}



	/*
	 * vuelvo a regrabar diccio de comentarios e index de terminos utiles
	 *
	 */

	for (i=0; i< ql; i++)
	{
		fprintf (hfout,"%s\n",cpt[i]);
	}
		
	for (i=0; i< q_cid; i++)
	{
		fprintf (hfou2,"%s\n",cid[i]);
	}
		


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 7
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *
 *	prue7
 *
 *	probamos algunas cosas para las redes neuronales
 *
 */


/* bloque */
#if 1


int	fim1(m1)
int	m1[100][100];
{
	int	i,j,k;
	
	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			m1[i][j] = i * 10000 + j;
}

int	fim2(m0)
void	*m0;
{
	int	i,j,k;
	int	*m1;

	m1 = (int *)m0;
	
	
	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			m1[i*100+j] = i * 10000 + j;
}


int	m2[100][100];


int	pro_prue7()
{
	int	i,j,k;
	int	f1,f2,f3;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;



	char	z[MAXV];
	sprintf (z,"prue7");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

#if 0
	if (!1 || !2 )
		gp_uso(11);
#endif


	/* bloque */

	fim1(m2);

	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			printf ("%2d %2d %5d %5d\n", i,j,i * 10000 + j, m2[i][j]);
		
	/* proceso */
	fim2(m2);

	for (i=0; i<100; i++)
		for (j=0; j<100; j++)
			printf ("%2d %2d %5d %5d\n", i,j,i * 10000 + j, m2[i][j]);
		
	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */






/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 8 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue 8 
 *
 *	abre archivo con lista de archivos a procesar 
 *	x cada archivo, abre y carga a memoria en vector de estructuras
 *	deja listo todo el contenido de fuentes en memoria para procesos
 *	termina y vuelve a grabar los archivos con mismo nombre, en otro dir
 *
 *	para buscar variables fortran que tienen blancos intermedios
 */


#if 0

#define	MAX_QSRC		500	/* cant max de archivos fuentes a manejar */
int	qf_ff;

typedef	struct tff	*ffptr;
typedef	struct tff
{	char	n[MAXB];		/* nombre de file */
	int	pf,uf;			/* primera - ultima fila */
	int	f1,f2,f3;		/* flags prop general */
}	ff;

ffptr	ffp1,ffp2,*ffq1,*ffq2;		/* punteros varios */

ffptr	tb[MAX_QSRC];

#endif


int	pro_prue8()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];


	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"prue8");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffdat )
		gp_uso(104);



	/* cantidad de archivos y lineas totales cargadas  */
	qf_ff = 0;
	q_ptr = 0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			fnq1 = &fnp[q_ptr];
			qfv_load(hwi,fnq1,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n\n",qlf,d1);


			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(904);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).pf = q_ptr;
			(*tb[qf_ff]).uf = q_ptr+qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d |%s|\n",
					(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
		}
	}


	/* cantidad de lineas totales en vector (global) */
	qf_src = q_ptr;

	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de archivos cargados :  %5d \n",qf_ff);
		printf ("Cantidad de lineas cargadas   :  %5d \n",q_ptr);
		printf ("\n");
	}

#if 1
	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< q_ptr; i++)
		{
			printf ("i: %5d  |%s| \n",
				i,(*fnp[i]).l );
		}
	}

	printf ("\n");

#endif

	/*
	 * A este punto, todas las lineas de archivos cargados en vector
	 * Hay otro vector, con nombre y lineas desde/hasta para indentificar
	 * a que archivo pertenece una linea determinada 
	 *
	 */

#if 0
	pr8_p1();
#endif



	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));

		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(609);

		for (j = (*tb[i]).pf ; j<= (*tb[i]).uf; j++)
		{
			fprintf (hwo,"%s\n", (*fnp[j]).l );
		}
	}


	fclose(hwo);

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}







/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_prue 9 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	prue 9 
 *
 *	abre achivo de pruebas (salida de pro_exec6 ... )
 *	con lineas de codigo
 *	prueba la rutina de chatgpt para identificar variables
 */


int	pro_prue9()
{

	int	i,j,k,flag;
	int	ql,qlf,q_ptr;
	int	n1;
	char	d1[MAXB];
	char	d2[MAXB];
	char	b1[MAXB];
	char	*v_var[100];
	int	l_var[100];

	FILE	*hwi,*hwo;

	char	z[MAXV];
	sprintf (z,"prue9");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout  )
		gp_uso(104);


	/* cantidad de archivos y lineas totales cargadas  */
	ql = 0;

	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j >= 0; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			/* verifico cuantas variables tengo */
			n1 = 0;
			rutina_w(b1, &n1, l_var,v_var);

			printf ("L: (%3d) #: %d |%s| \n",ql,n1,b1);


			for (int i = 0; i < n1; i++) 
			{
				strncpy(d1,v_var[i],l_var[i]);
				d1[l_var[i]] = 0;

			}

			ql++;
		}
	}



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



int	rutina_w(s,count,v_len,v_pos)
char	*s;
int	*count;
int	*v_len;
char	**v_pos;
{

	*count = 2;

	v_len[0]= 4;
	v_pos[0]= s+10;
	v_len[1]= 4;
	v_pos[1]= s+20;
}















/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_proc 1 
 *
 *	procesos adicionales / alternativos
 *
 * -----------------------------------------------------------------------------------
 */

/*	WIP
 *
 *	proc 1
 *
 *	carga el fuente fortran
 *	
 *	busca en las lineas
 *	INCLUDE algo
 *	USE algo
 */ 


int	pro_proc1()
{
	char	z[MAXV];
	sprintf (z,"proc1");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!!ffsrc )
		gp_uso(111);

	/* cargo el source en memoria */
	pf_load();

	/* busco expresiones del tipo 'USE ... ' */
	busco_use();

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_proc 2 
 *
 *	procesos adicionales / alternativos
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	proc 2
 *
 *	carga todos los fuentes fortran a partir de un
 *	archivo listado
 */ 



int	pro_proc2()
{

	char	b1[MAXB];
	char	b2[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;
	int	ql_arc;

	char	farc[MAXF];
	int	ffarc;
	FILE	*hfarc;

	char	z[MAXV];
	sprintf (z,"proc2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

/* proc2 */
#if 1

	if (!!fflst)
		gp_uso(112);


/*
 *	todo esto aca, esta mal
 *	hay que poner solo las llamadas a cada archivo para procesar 
 *	el esto, en subrutinas
 *
 */

	ffarc=1;

	ql=0;
	while (fgets(b1,MAXB,hflst) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			strcpy(farc,b1);

			if (gp_fverbose("d1"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}


			if ( ffarc && ((hfarc = fopen (farc,"r")) == NULL) )
			{
				error(201);
			}


			/* proceso file ... aca ??? */

			ql_arc = 0;

			while (fgets(b2,MAXB,hfarc) != NULL)
			{
				/* saco el fin de linea - contemplo 13 x fuentes fortran */
				for ( flag=0, j=strlen(b2); !flag && j; j--)
					if (b2[j] == '\n' )
					{	
						flag=1;

						if ( j && b2[j-1] == 13)
							b2[j-1]=0;
						else
							b2[j]=0;
					}

				printf ("|%s|\n",b2);
				ql_arc++;

			}



			/* cierro file */
			if (ffarc)
				fclose(hfarc);

			ql++;


		}  /* if linea vacia ... */
	}




#endif
/* proc2 */

		

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *          
 *
 *	pro_proc 3 
 *
 *	procesos adicionales / alternativos
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	proc 3
 *
 *	carga el fuente fortran
 *	
 *	a ver si podemos generar uno con correccion de continuacion de lineas
 *
 *	./prog  -v -f -opciones=d5 -proc=3 -src=input -out=output
 *
 */ 


int	pro_proc3()
{
	sprintf (gp_p,"proc3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_e,gp_p,gp_tm());
	}

#if 1
	/* cargo el source en memoria */
	pf_load();



	/* grabo el source a file */
	pf_write();

#endif


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_p,gp_p,gp_tm());
	}
}




/*
 * -----------------------------------------------------------------------------------
 *          
 *
 *	pro_proc 4 
 *
 *	carga un conjunto de fuentes sobre nodos estruct encadenados
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	proc 4
 *
 *	carga todos los fuentes en archivo lista
 *	
 *
 */ 


int	pro_proc4()
{
	int	i,j,k;
	int	flag;
	int	ql,qlf;
	int	q_ptr;
	char	d1[MAXB];
	char	d2[MAXB];
	FILE	*hwi,*hwo;


	char	z[MAXV];
	sprintf (z,"proc4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

/* proc4 */
#if 1

	if (!ffinp || !ffout || !ffdat )
		gp_uso(113);


	/* cantidad de lineas en el archivo  */
	qf_ff = 0;
	q_ptr = 0;
	ql=0;

	while (fgets(d1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(d1)  && d1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(d1); !flag && j >= 0; j--)
				if (d1[j] == '\n' )
				{	
					flag=1;
					if ( j && d1[j-1] == 13)
						d1[j-1]=0;
					else
						d1[j]=0;
				}

			/* proceso file */
			if (gp_fverbose("d3"))
				printf ("Archivo a cargar:  |%s|\n",d1);

			if ( 1 && ((hwi = fopen (d1,"r")) == NULL) )
				error(601);

			qfc_load(hwi,ql,&qlf);

			fclose (hwi);

			/* procese file */
			if (gp_fverbose("d3"))
				printf ("Archivo cargado:  %5d |%s|\n",qlf,d1);

			/* registro datos del archivo */
			tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
			if ( tb[qf_ff] == NULL )
				error(905);

			strcpy ( (*tb[qf_ff]).n, extract_fname(d1));
			(*tb[qf_ff]).ql = qlf;
			(*tb[qf_ff]).pf = 0;
			(*tb[qf_ff]).uf = qlf-1;

			if (gp_fverbose("d1"))
			{
				printf ("load: %5d %5d %5d |%s|\n\n",
					(*tb[qf_ff]).ql,(*tb[qf_ff]).pf,(*tb[qf_ff]).uf,(*tb[qf_ff]).n);
			}

			qf_ff++;
			q_ptr += qlf;
			ql++;
		}
	}


	if (gp_fverbose("d3"))
	{
		printf ("Cantidad de files/lineas cargados:  %5d %5d \n",ql,q_ptr);
		printf ("\n");
	}

	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< qf_ff; i++)
		{
			printf ("i: %5d  %5d |%s| \n",
				i,(*tb[i]).ql,(*tb[i]).n );
		}
	}

	printf ("\n");


	/* proceso todos los files */
#if 1
	ps_src1();
#endif


	/* grabo new file */
	for (i = 0; i < qf_ff; i++)
	{
		/* nombre del archivo de salida */
		sprintf (d2,"%s/%s",gp_dato,extract_fname( (*tb[i]).n));


		if ( 1 && ((hwo = fopen (d2,"w")) == NULL) )
			error(604);

#if 1
		qfc_write(hwo,i);
#endif
	}

	fclose(hwo);
		


#endif
/* proc4 */


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}






/*
 * -----------------------------------------------------------------------------------
 *
 *	carga un archivo a vector de estructuras
 *
 * -----------------------------------------------------------------------------------
 */



/* qfv_load */
#if 1

int	qfv_load(pfr,q1,l1)
FILE	*pfr;
fnptr	*q1;
int	*l1;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	char	z[MAXV];
	sprintf (z,"qfv_load");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* cantidad de lineas en el archivo  */
	ql=0;

	while (fgets(b1,MAXB,pfr) != NULL)
	{

#if 0
		if (!linea_vacia(b1)  && b1[0] != '#' )
#endif
		if ( 1 )
		{

			if (b1[strlen(b1)-1] == '\r' )
				b1[strlen(b1)-1] = 0;

			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j >= 0; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


#if 0
			if (gp_fverbose("d3"))
				printf ("%3d |%s|\n",ql,b1);
#endif


			q1[ql] = (fnptr  ) malloc (sizeof (node));
			if ( q1[ql] == NULL)
				error(901);

			sprintf ( (*q1[ql]).l,"%s",b1);
			(*q1[ql]).f1 = 0;
			(*q1[ql]).f2 = 0;
			(*q1[ql]).f3 = 0;

			ql++;
		}
	}

	*l1 = ql;

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas en source ql       : %6d\n",ql);
		printf ("\n\n");
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}

#endif
/* qfv_load */






/*
 * -----------------------------------------------------------------------------------
 *
 *	carga un archivo a cadena de estructuras
 *
 * -----------------------------------------------------------------------------------
 */



/* qfc_load */
#if 1

int	qfc_load(pfr,q1,l1)
FILE	*pfr;
int	q1;
int	*l1;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	char	z[MAXV];
	sprintf (z,"qfc_load");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* cantidad de lineas en el archivo  */
	ql=0;





	ldfq1 = (ldf_ptr *) &vldf[q1];



	while (fgets(b1,MAXB,pfr) != NULL)
	{

#if 0
		if (!linea_vacia(b1)  && b1[0] != '#' )
#endif
		if ( 1 )
		{

			if (b1[strlen(b1)-1] == '\r' )
				b1[strlen(b1)-1] = 0;

			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			if (gp_fverbose("d3"))
				printf ("%3d |%s|\n",ql,b1);

			*ldfq1 = (ldf_ptr  ) malloc (sizeof (ldf_node));
			if ( *ldfq1 == NULL)
				error(909);

			sprintf ( (**ldfq1).l,"%s",b1);
			(**ldfq1).f1 = 0;
			(**ldfq1).f2 = 0;
			(**ldfq1).f3 = ql;
			(**ldfq1).nx = (ldf_ptr) NULL;
			ldfq1 = (ldf_ptr *) & (*ldfq1)->nx;

			ql++;
		}
	}

	*l1 = ql;

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas en source ql       : %6d\n",ql);
	}

	if (gp_fverbose("d4"))
	{

		i=0;
		ldfq1 = (ldf_ptr *) &vldf[q1];

		while ( (*ldfq1) != (ldf_ptr) NULL )
		{
			printf ("i: %3d  (**ldfq1).f3: %3d  (**ldfq1).l: |%s|\n",i,(**ldfq1).f3,(**ldfq1).l);
			ldfq1 = (ldf_ptr *) & (*ldfq1)->nx;
			i++;
		}


	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}

#endif
/* qfc_load */





/*
 * -----------------------------------------------------------------------------------
 *
 *	graba un fuente en cadena de structs a file 
 *
 * -----------------------------------------------------------------------------------
 */



/* qfc_write */
#if 1

int	qfc_write(pfr,q1)
FILE	*pfr;
int	q1;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	char	z[MAXV];
	sprintf (z,"qfc_write");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	i=0;
	ldfq1 = (ldf_ptr *) &vldf[q1];

	while ( (*ldfq1) != (ldf_ptr) NULL )
	{
		fprintf (pfr,"%s\n", (**ldfq1).l );

		if (gp_fverbose("d4"))
		{
			if (i == 0)
				printf ("Grabo fuente nro: %5d\n",q1);


			printf ("GRA: i: %3d  (**ldfq1).f3: %3d  (**ldfq1).l: |%s|\n",i,(**ldfq1).f3,(**ldfq1).l);
		}

		ldfq1 = (ldf_ptr *) & (*ldfq1)->nx;
		i++;
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}

#endif
/* qfc_write */






/*
 * -----------------------------------------------------------------------------------
 *
 *	agrega una estructura al vector de estructuras (qfv_load)
 *
 * -----------------------------------------------------------------------------------
 */



int	qf_addline(q1,nl)
fnptr	*q1;
int	nl;
{
	q1[nl] = (fnptr  ) malloc (sizeof (node));
	if ( q1[nl] == NULL)
		error(911);

	(*q1[nl]).l[0] = 0;
	(*q1[nl]).f1 = 0;
	(*q1[nl]).f2 = 0;
	(*q1[nl]).f3 = 0;
}












/*
 * -----------------------------------------------------------------------------------
 *
 *	carga de un archivo a vector
 *
 * -----------------------------------------------------------------------------------
 */


/* pf_load */
#if 1

int	pf_load()
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	/* cantidad de lineas en el archivo  */
	qf_lin = 0;
	ql=0;

	while (fgets(b1,MAXB,hfsrc) != NULL)
	{
#if 0
		if (!linea_vacia(b1)  && b1[0] != '#' )
#endif
		if ( 1 )
		{

			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;

					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			if (gp_fverbose("d1"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}

			fnp[ql] = ( fnptr ) malloc ( sizeof (node));
			if ( fnp[ql] == NULL)
				error(902);

			sprintf ( (*fnp[ql]).l,"%s",b1);
			(*fnp[ql]).f1 = 0;
			(*fnp[ql]).f2 = 0;
			(*fnp[ql]).f3 = 0;

			qf_lin++;
			ql++;
		}
	}

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas en source ql       : %6d\n",ql);
	}


	if (gp_fverbose("d3"))
	{
		printf ("\n\nComprobando integridad de la carga: \n\n");
	
		for ( i=0; i< ql; i++)
		{
			printf ("i: %3d  (*fnp[i]).fx  %2d %2d %2d  (**fnp[i]).l: |%s| \n",
				i,(*fnp[i]).f1, (*fnp[i]).f2, (*fnp[i]).f3, (*fnp[i]).l );
		}

		printf ("\n");
	}
}

#endif
/* pf_load */








/*
 * -----------------------------------------------------------------------------------
 *
 *	graba un archivo de vector
 *
 * -----------------------------------------------------------------------------------
 */


/* pf_write */
#if 1



int	pf_write()
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	/* cantidad de lineas en el archivo  */


	for (i=0; i< qf_lin; i++)
	{
		fprintf (hfout,"%s\n", (*fnp[i]).l );
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de lineas en source qf_lin   : %6d\n",qf_lin);
	}


}

#endif
/* pf_write */




















/*
 * -----------------------------------------------------------------------------------
 *
 *	busco_use
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	qf_lin		tiene la cantidad de lineas en el source
 *	(*fnp[i]).l	tiene la linea
 *	(*fnp[i]).f1	tiene valor aux f1
 *	(*fnp[i]).f2	tiene valor aux f2
 *	(*fnp[i]).f3	tiene valor aux f3
 *
 */


int	busco_use()
{


	int	i,j,k;
	char	b1[MAXR];
	char	b2[MAXR];


	printf ("Entro a busco_use, qf_lin  %d\n\n",qf_lin);

	for (i=0; i< qf_lin; i++)
	{
		strcpy(b1,(*fnp[i]).l );
		strcpy(b2,"set original");

		k = 0;
		k = encuentro_use(b1,b2);


		if ( k )
		{
			printf ("Encontre en lin: %6d \n",i);
			printf ("|%s|\n",b1);
			printf ("|%s|\n",b2);
			printf ("- - - \n");
		}
	}

	printf ("Salgo de busco_use \n\n");
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	encuentro_use
 *
 * -----------------------------------------------------------------------------------
 */




int	encuentro_use(s1,s2)
char	*s1;
char	*s2;
{
	char b1[MAXR];
	static char b2[MAXR];

	int	p1,p2,p3;
	int	flag;


	strcpy(b1,pasar_a_minusc(s1));
	strcpy(b2,"_");
	flag = 0;

	for (p1=0; b1[p1]; p1++)
	{

		if (!linea_comentario(b1))
		{


		if (!strncmp(b1+p1,"use",3) )
		{
			flag = 1;
			p1 = p1 + 3;
			p2 = 0;

			while (b1[p1] == ' ' || b1[p1] == 9 )
			{	p1++;
				if (p1 == MAXR )
					error (502);
			}
			
			while ( is_use_valid( b1[p1+p2] ) ) 	/* condicion de palabra valida para nombre de use */
			{
				p2++;
				if (p2 == MAXR)
					error (503);
			}

			strncpy (b2,b1+p1,p2);
			strncpy (s2,b1+p1,p2);

			if (gp_fverbose("d1"))
			{
				printf ("test: \n");
				printf ("b1 |%s| \n",b1);
				printf ("b2 |%s| \n",b2);
				printf ("s2 |%s| \n",s2);
				printf ("p1 %d \n",p1);
				printf ("p2 %d \n",p2);
				printf ("- - - \n");
			}


		}

		}


	}


	return flag;
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	encuentro_use
 *
 * -----------------------------------------------------------------------------------
 */

int	linea_comentario(s)
char	*s;
{
	int flag;

	flag = 0;

	if (s[0] == 'C' || s[0] == 'C')
		flag = 1;

	if (s[0] == '!' )
		flag = 1;

	return flag;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	is_use_valid
 *
 *	funcion para determinar si caracter es valido en nombre para 'use'
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	is_use_valid(c)
char	c;
{
	int x;

	x=0;
	if ( c >= 'a' && c <= 'z' )
		x=1;
	if ( c >= '0' && c <= 'z' )
		x=1;
	if ( c == '_' || c == '-' )
		x=1;


	return x;
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	pro_tool  1
 *
 *	tools  ...
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *
 *	tool 1
 *
 *	carga dos listas l1 l2, genera listados l2 en l1 l_si l_no
 *	archivos inp / out
 *	inp list_make    (listado de fuentes en make )
 *	in2 list_src     (listado de src en repo )
 *	out l_si         (listado de lineas en l2 que SI estan en l1)
 *	ou2 l_no         (listado de lineas en l2 que NO estan en l1)
 * 	
 *	-m               las comparaciones se hacen en lower case
 *                       (pero los listados se generan con case original )
 */



int	pro_tool1()
{
	int	i,j,k;
	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool1");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* chequeo de parametros minimos */
	if (!ffinp || !ffin2 || !ffout || !ffou2 )
		gp_uso(114);


	/* cargo lista l1 */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&qf_lin);

	if (gp_fverbose("d3") )
	{
		for (i=0; i< qf_lin; i++)
			printf ("%3d |%s|\n", i, fnp[i]->l );
	}

	/* cargo lista l2 */
	fnq2 = &fnf[0];
	qfv_load(hfin2,fnq2,&qf_fen);

	if (gp_fverbose("d3") )
	{
		for (i=0; i< qf_fen; i++)
			printf ("%3d |%s|\n", i, fnf[i]->l );
	}


	if (gp_fverbose("d3"))
	{
		printf ("\n");
		printf ("Cant de lineas lista 1 %4d\n",qf_lin);
		printf ("Cant de lineas lista 2 %4d\n",qf_fen);
		printf ("\n");
	}

	/* revisa lista 2 ... si hay paths u otras cosas ademas del nombre */
	check_fnames();

	/* revisa las condiciones esta/ no esta l2 en l1 */
	check_c1();

	/* graba resultados */
	write_result();

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}

}



/*
 * -----------------------------------------------------------------------------------
 *
 *	check_fnames
 *
 *	revisa si hay otras cosas ademas del nombre de archivo
 *
 * -----------------------------------------------------------------------------------
 */


int	check_fnames()
{
	int	i,j,k;
	int	f1,f2;
	char	b1[MAXB];
	char	b2[MAXB];

	char	z[MAXV];
	sprintf (z,"check_names");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		
	for (i=0; i< qf_fen; i++)
	{
		strcpy (  (*fnf[i]).l , extract_fname ( (*fnf[i]).l )  );
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}





/*
 * -----------------------------------------------------------------------------------
 *
 * 	extract_fname
 *
 * -----------------------------------------------------------------------------------
 */

char	*extract_fname(s)
char	*s;
{
	static char b[MAXB];
	int 	i,j,k;

	strcpy(b,s);
	for (i=strlen(b) - 1, j=0; i>= 0 && !j; i-- )
		if ( b[i] == ' ' || b[i] == '\\' || b[i] == '/' )
			j=i;

	if (j==0)
		k=0;
	else	
		k=j+1;

	return b+k;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	write_result
 *
 *	graba resultados de la comparacion de listas
 *
 * -----------------------------------------------------------------------------------
 */

int	write_result()
{
	int	i,j,k;
	int	f1,f2;
	char	b1[MAXB];
	char	b2[MAXB];

	char	z[MAXV];
	sprintf (z,"write_result");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		
	for (i=0; i< qf_fen; i++)
	{
		if ( (*fnf[i]).f1 == 1)
			fprintf (hfout,"%s\n", (*fnf[i]).l );
		if ( (*fnf[i]).f1 == 2)
			fprintf (hfou2,"%s\n", (*fnf[i]).l );
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}	





/*
 * -----------------------------------------------------------------------------------
 *
 *	check_c1
 *
 *	verifica condiciones entre dos listas 
 *
 * -----------------------------------------------------------------------------------
 */



int	check_c1()
{
	int	i,j,k;
	int	f1,f2;
	char	b1[MAXB];
	char	b2[MAXB];

	char	z[MAXV];
	sprintf (z,"check_c1");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}
		

	for (i=0; i< qf_fen; i++)
	{
		for (j=0, f1=1; f1 && j< qf_lin ; j++)
		{
			if (gp_minusculas)
			{
				strcpy(b1,pasar_a_minusc ( (*fnp[j]).l ) );
				strcpy(b2,pasar_a_minusc ( (*fnf[i]).l ) );
			}
			else
			{
				strcpy(b1, (*fnp[j]).l );
				strcpy(b2, (*fnf[i]).l );
			}

			if (gp_fverbose("d4"))
			{
				printf ("cmp: |%s| |%s| \n",b1,b2);
			}


			if (!strcmp ( b1 , b2 ) )
			{
				(*fnf[i]).f1 = 1;
				f1 = 0;

				if (gp_fverbose("d3"))
					printf ("esta si: |%s| \n", (*fnf[i]).l );
			}
		}

		if (f1 && j == qf_lin)
		{
			(*fnf[i]).f1 = 2;

			if (gp_fverbose("d3"))
				printf ("esta no: |%s| \n", (*fnf[i]).l );
		}
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool  2
 *
 *	tools  ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool 2
 *
 *	descrip
 *
 */

int	pro_tool2()
{
	int	i,j,k;
	int	ql;
	int	c1;
	int	f_sigo;
	char	b0[MSTR];
	char	b1[MAXB];
	char	b2[MAXB];


	char	z[MAXV];
	sprintf (z,"tool2");



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* chequeo condiciones para seguir */
	if (!ffinp || !ffout )
		gp_uso(115);


	if (gp_suboption == 0)
	{
		/* cargo mk a memo */
		load_makefile(hfinp);

		/* proceso mk */
		proc_makefile();
	
		/* elimino dups */
		filter_makefile();
	
		/* ordeno */
		ordenar_makefile();
	
		/* pidio minusculas */
		if (gp_minusculas)
			to_min();

		/* grabo listado de files encontrados */
		write_makefile(hfout);
	}


	if (gp_suboption == 1)
	{
		/* cargo mk a memo */
		load_makefile2(hfinp);

		/* carga lista de files a los qye hay que agregar allocate_vars.obj */
		cargar_lista_files(&ql);

		c1 = 0;

		for (i=0; i<ql; i++)
		{
			strcpy(b1,pasar_a_minusc( tb_x1[i]) );
			for (k=0; k<strlen(b1); k++)
				if (b1[k] == '.')
					b1[k]=0;
		

			f_sigo = 1;
			for (j=0; f_sigo && j<qf_lin; j++)
			{
printf ("comparo: \n");
printf ("1|%s|\n", (*fnpm[j]).l);
printf ("2|%s|\n", b1);
printf (" .... \n");
				strcpy(b0,pasar_a_minusc( (*fnpm[j]).l ));

				if (!strncmp( b0 , b1, strlen(b1) ))
				{
					strcat( (*fnpm[j]).l," allocate_vars.obj");
					c1++;
					f_sigo = 0;
				}

			}

			if (f_sigo == 1)
				printf ("no encontre: |%s|\n",b1);

		}


		/* grabo new makefile */
		for (i=0; i<qf_lin; i++)
			fprintf (hfout,"%s\n",(*fnpm[i]).l );


		printf ("Cantidad de objs que req agregar     %3d \n",ql);
		printf ("Cantidad de allocate_vars agregados  %3d \n",c1);

	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	cargar_lista_files
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * carga lista de nombres de a los que hay que agregar allocate_vars.obj
 *
 */

int	cargar_lista_files(ql)
int	*ql;
{
	int	i,j,k;
	int	flag;
	int	qlin;
	char	b1[MAXB];
	char	b2[4];

	char	z[MAXV];
	sprintf (z,"cargar_lista_files");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	qlin = 0;
	while (fgets(b1,MAXB,hfin2) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			strcpy(tb_x1[qlin],b1);
			qlin++;
		}
	}

	if (gp_fverbose("d2"))
	{
		printf ("Carga de lista files a agregar allocate_vars \n");
		for (j=0; j<qlin; j++)
			printf ("%2d  |%s| \n",j,tb_x1[j]);
		printf ("\nTotal de lineas cargadas :  %d \n",qlin);
	}


	*ql = qlin;

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	make_cmp
 *	compara dos elementos del vector de estrcturas para el qsort 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * lo que esta en el sprintf anda ??????
 */


int	make_cmp(fnp1,fnp2)
fnptr	*fnp1;
fnptr	*fnp2;
{
	static	char	b1[MAXB];
	static	char	b2[MAXB];
	int	k;

#if 0
	sprintf (b1,"%2d%-40-40s", (*fnp1)->f1, (*fnp1)->l );
	sprintf (b2,"%2d%-40-40s", (*fnp2)->f1, (*fnp2)->l );
#endif
	sprintf (b1,"%2d%-40.40s", (*fnp1)->f1, (*fnp1)->l );
	sprintf (b2,"%2d%-40.40s", (*fnp2)->f1, (*fnp2)->l );



	k = strcmp(b1,b2);

	if (gp_fverbose("d4"))
	{
		printf ("SSS make_cmp %s %s %d !! \n",b1,b2,k);
	}

	return k;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	ordenar_makefile
 *	ordena alfabeticamente los nombres que encontro en el makefile
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	le estamos pasando a qsort
 *	vector de punteros a estructuras
 *	cantidad de elementos en el vector (que hay que ordenar)
 *	tamano de cada elemento (es un puntero a estructura)
 *	la funcion de comparacion
 *
 */


int	ordenar_makefile()
{
	qsort ( &fnf[0],qf_fen,sizeof( fnptr) ,make_cmp );
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	load_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */

#if 0


/*
 * 	Variables y estructs para cargar todo un archivo en memoria
 *
 */


#define	MAX_FSRC	10000		/* 10000 lineas de codigo ... seran suf ? */
int	qf_lin;				/* lineas en file */

int	pf_load();			/* proceso de carga del file */
int	pf_write();			/* proceso de write del file */

typedef	struct	tfn	*fnptr;
typedef	struct	tfn
{	char	l[MAXR];		/* por ahora, despues malloc */
	int	f1;			/* usos varios */
	int	f2;
	int	f3;
}	node;

fnptr	fnp1,fnp2,*fnpa;

fnptr	fnp[MAX_FSRC];			/* vector de punteros a lineas de source */


#endif

/*
 *
 *	load_makefile
 *
 *	descrip
 *
 */

int	load_makefile(fpr)
FILE	*fpr;
{
	char	b1[HUG2];
	int	flag,f1;
	int	i,j,k;
	int	q_mk;
	int	ql;
	int	mas_l;


	char	z[MAXV];
	sprintf (z,"load_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


#if 1

	/* cantidad de lineas en file  */
	mas_l=0;
	q_mk = 0;
	ql   = 0;

	qf_lin=0;

	while (fgets(b1,HUG2,fpr) != NULL)
	{

		/* temita de los archivos con line feed */
		if (b1[strlen(b1)-2] == 13)
			b1[strlen(b1)-2] = 32;

		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n')
					b1[j]=0,flag=1;

			if (gp_fverbose("d3"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}


			if (strlen(b1) > mas_l)
				mas_l = strlen(b1);


			fnp[qf_lin] = ( fnptr ) malloc ( sizeof (node));
			if ( fnp[qf_lin] == NULL)
				error(908);

			sprintf ( (*fnp[qf_lin]).l,"%s",b1);
			(*fnp[qf_lin]).f1 = 0;
			(*fnp[qf_lin]).f2 = 0;
			(*fnp[qf_lin]).f3 = 0;
			

			qf_lin++;
			q_mk++;
		}
		ql++;
	}

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas cargadas : %6d\n",ql);
		printf ("Cantidad de lineas utiles   : %6d\n",q_mk);
		printf ("Linea mas larga encontrada  : %6d\n",mas_l);
		printf ("\n");
	}

#if 1

	if (gp_fverbose("d4"))
	{
		printf ("\n\nComprobando integridad del makefile : \n\n");

		for (i=0; i< qf_lin; i++)
		{
			printf ("%04d |%s|\n",i,(*fnp[i]).l );
		}
		printf ("\n");
	}

#endif



#endif


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	load_makefile2  ... entero con lineas en blanco y comentarios 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	load_makefile2
 *
 *	descrip
 *
 */

int	load_makefile2(fpr)
FILE	*fpr;
{
	char	b1[MAXM];
	int	flag,f1;
	int	i,j,k;
	int	q_mk;
	int	ql;
	int	mas_l;


	char	z[MAXV];
	sprintf (z,"load_makefile2");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


#if 1

	/* cantidad de lineas en file  */
	mas_l= 0;
	q_mk = 0;
	ql   = 0;

	qf_lin=0;

	while (fgets(b1,MAXM,fpr) != NULL)
	{

		/* temita de los archivos con line feed */
		if (b1[strlen(b1)-2] == 13)
			b1[strlen(b1)-2] = 32;

		if (1)
		{
			if (strlen(b1) >= 2)
			{
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n')
					b1[j]=0,flag=1;
			}
			else
				b1[0]=0;


			if (gp_fverbose("d3"))
			{
				printf ("%3d |%s|\n",ql,b1);
			}


			if (strlen(b1) > mas_l)
				mas_l = strlen(b1);


			fnpm[qf_lin] = ( fnptrm ) malloc ( sizeof (nodem));
			if ( fnpm[qf_lin] == NULL)
				error(908);

			sprintf ( (*fnpm[qf_lin]).l,"%s",b1);
			(*fnpm[qf_lin]).f1 = 0;
			(*fnpm[qf_lin]).f2 = 0;
			(*fnpm[qf_lin]).f3 = 0;
			

			qf_lin++;
			q_mk++;
		}
		ql++;
	}

	if (gp_fverbose("d2"))
	{
		printf ("Cantidad de lineas cargadas : %6d\n",ql);
		printf ("Cantidad de lineas utiles   : %6d\n",q_mk);
		printf ("Linea mas larga encontrada  : %6d\n",mas_l);
		printf ("\n");
	}

#if 1

	if (gp_fverbose("d4"))
	{
		printf ("\n\nComprobando integridad del makefile : \n\n");

		for (i=0; i< qf_lin; i++)
		{
			printf ("%04d |%s|\n",i,(*fnpm[i]).l );
		}
		printf ("\n");
	}

#endif



#endif


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	proc_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	proc_makefile
 *
 *	tomo archivo formato makefile y extraigo todos los files en dependencias
 *	
 *	qf_fen    es la cantidad de files extraigs 
 */

int	proc_makefile()
{
	int	i,j,k;
	int	f1,f2,f3;
	int	p1,p2,p3;
	int	mx;
	char	b1[HUG2];
	char	b2[MAXB];


	char	z[MAXV];
	sprintf (z,"proc_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	qf_fen = 0;


	for (i=0; i< qf_lin; i++ )
	{
		strcpy(b1,(*fnp[i]).l );
		mx=strlen(b1);
	
		f1=1;
		j=0;

		while (f1)
		{
			if (es_char_v (b1[j]) )
			{
				f2=1;
				p1=0;

				memset(b2,0,sizeof(b2));

				while(f1 && f2)
				{
					if(es_char_v(b1[j]) )
					{
						b2[p1] = b1[j];
						p1++;
						j++;
						if (j == mx)
							f2=0,f1=0;
					}
					else
					{	f2=0;
						if (gp_fverbose("d3") )
						{
							printf ("encontre: |%s| \n",b2);
						}
						if (es_str_v(b2))
						{
#if 0
							fprintf (fpr,"%s\n",b2);	
#endif
							fnf[qf_fen] = ( fnptr ) malloc ( sizeof (node));
							if ( fnf[qf_fen] == NULL)
								error(906);

							sprintf ( (*fnf[qf_fen]).l,"%s",b2);
							(*fnf[qf_fen]).f1 = 1;	
							(*fnf[qf_fen]).f2 = 0;
							(*fnf[qf_fen]).f3 = 0;

							qf_fen++;
							
						}
					}
				}	
			}
			else
			{
				j++;
				if (j == mx)
					f1=0;
			}

		}
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	write_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */


int	write_makefile(fpr)
FILE	*fpr;
{
	int	i,jmk;

	char	z[MAXV];
	sprintf (z,"write_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	for (i=0; i< qf_fen; i++)
	{
		if ( (*fnf[i]).f1   )
		{
			fprintf (fpr,"%s\n",(*fnf[i]).l);
		}
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	to_min   ...
 *
 * -----------------------------------------------------------------------------------
 */


int	to_min()
{

	int	i,j,k;
	int	f1,f2;

	char	z[MAXV];
	sprintf (z,"to_min");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* to min */
	for (i=0; i< qf_fen; i++)
	{
		strcpy( (*fnf[i]).l,  pasar_a_minusc( (*fnf[i]).l )  );
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	filter_makefile  ...
 *
 * -----------------------------------------------------------------------------------
 */


int	filter_makefile()
{

	int	i,j,k;
	int	f1,f2;

	char	z[MAXV];
	sprintf (z,"filter_makefile");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


#if 1

	i=0;
	f1=1;

	while (f1)
	{
		if( (*fnf[i]).f1 )
		{
			for (j=i+1; j< qf_fen; j++)
			{
				if (gp_fverbose("d4"))
				{
					printf ("cmp: i %3d j %3d  s1-s2   |%s|  |%s| \n",
						i,j,(*fnf[i]).l,(*fnf[j]).l );
				}

				if ( !strcmp( (*fnf[i]).l , (*fnf[j]).l )  )
					(*fnf[j]).f1 = 0;
			}
		}

		i++;
		if (i == qf_fen)
			f1=0;
	}

#endif

	if (gp_fverbose("d4"))
	{
		for (i=0; i<qf_fen; i++)
		{
			printf ("RRR: %4d %2d |%-40.40s| \n",i, (*fnf[i]).f1, (*fnf[i]).l );
		}
	}



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}


/*
 * -----------------------------------------------------------------------------------
 *
 *	es_char_v  ...
 *
 * -----------------------------------------------------------------------------------
 */




int	es_char_v(c)
char	c;
{

	int x;

	x=0;

	if (c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z' )
		x = 1;

	if (c == '_' || c == '.' )
		x = 1;

	if (c >= '0' && c <= '9' )
		x = 1;

#if 0
	/* agrego los que vienen de analizar fuentes fortran */
	if (c == '\'' || c == '%' || c == '!' || c == '*'  )
		x = TC_CVR;

	if (c == '"' || c == '&' || c == '+' || c == '=' || c == '>' || c == '<' || c == '?' )
		x = TC_CVR;
#endif

	return x;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	es_str_v  ...
 *
 * -----------------------------------------------------------------------------------
 */



int	es_str_v(s)
char	*s;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2;
	int	mx;
	int 	x;
	char	b1[MAXB];
	char	b2[MAXB];

	x=1;
	i=0;
	f1=1;
	strcpy(b1,s);
	mx=strlen(b1);

	/* el primer caracter no puede ser punto */
	j=0;

	if (f1)
	{
		if ( b1[j] == '.')
		{	x=0;
			f1=0;
		}
	}

	/* el ultimo caracter no puede ser punto */
	j=0;

	if (f1)
	{
		if ( b1[mx-1] == '.')
		{	x=0;
			f1=0;
		}
	}

	/* tiene q tener un punto */
	j=0;

	if (f1)
	{
		for (k=0; k< mx; k++)
		{
			if (b1[k] == '.')
			{	j++;
				strcpy(b2,b1+k);
			}
		}

		if (j != 1)
		{	x=0;
			f1=0;
		}
	}



	/* la extension no puede ser de la forma .x (largo 1)  */
	if (f1)
	{
		if ( strlen(b2) == 2 )
		{	x=0;
			f1=0;
		}
	}



	/* la extension debe ser una de ... */
	if (f1)
	{
		f2=0;

		if (!strcmp(b2+1,"for") )
			f2=1;
		if (!strcmp(b2+1,"FOR") )
			f2=1;
		if (!strcmp(b2+1,"f90") )
			f2=1;
		if (!strcmp(b2+1,"F90") )
			f2=1;
		if (!strcmp(b2+1,"f95") )
			f2=1;
		if (!strcmp(b2+1,"F95") )
			f2=1;
		if (!strcmp(b2+1,"mon") )
			f2=1;
		if (!strcmp(b2+1,"MON") )
			f2=1;

		if (!f2)
		{
			x=0;
			f1=0;
		}


	}

	/* si es archivo valido ... */
	if (f1)
	{
		if (gp_fverbose("d3"))
			printf ("extension: |%-40s| |%s|\n",b1,b2);
	}


	return x;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	pro_tool  3
 *
 *	tools  ...
 *
 * -----------------------------------------------------------------------------------
 */

/*




/*
 *
 *	tool 3
 *
 *	descrip
 *
 */

int	pro_tool3()
{
	int	i,j,k;
	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool3");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&qf_lin);


	for (i=0; i< qf_lin; i++)
	{
		strcpy(b1,pasar_a_minusc( (*fnp[i]).l ));

		if (gp_fverbose("d1"))
			printf ("%4d |%s| \n", i,b1 );

		fprintf (hfout,"%s\n",b1);
	}


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	tool4
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	Descrip
 *
 * 	carga un src en memoria, en vector a pnt de esctructs
 *
 *	- cambia el formato de declaracion de variables (migro a tool6)
 *
 */


#if 1

int	pro_tool4()
{
	int	i,j,k;
	int	ql_ini,ql_fin;
	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool4");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* que la vida sea mas leve */
	if (!ffaux)
		agregar_ffaux("parser.err");

	/* chequeamos reqs */
	if (!ffinp || !ffout || !ffaux )
		gp_uso(116);

	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&ql_ini);


	/* mientras que no cambie la cant de lineas !!! */
	ql_fin=ql_ini;

	
#if 0
	/* 1 - cambio los formatos de declaracion de variables */
	cfor_vars(&ql_ini,&ql_fin);
#endif


	/* grabo file */
	for (i=0; i< ql_fin; i++)
	{
		strcpy(b1, (*fnp[i]).l );

		if (gp_minusculas)
			strcpy( b1 , pasar_a_minusc( (*fnp[i]).l )  );

		fprintf (hfout,"%s\n",b1);
	}




	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

#endif







/*
 * -----------------------------------------------------------------------------------
 *
 *	check_file
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * Suponiendo que estamos en f77 ... 
 * check file verifica si hay alguna linea "valida" (obviando comentarios)
 * que sea mas larga que 72 , porque esa va a dar error al compilar !
 *
 */



int	check_file(ql_f)
int	*ql_f;
{

	int	i,j,k;
	int	l1,l2;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	qf;

	char	b1[MSTR];
	char	b2[MSTR];


	qf = *ql_f;

	/* f1 = 1 ... check_file ok !
	 *      0 ... fallo !
	 */

	f1 = 1;

printf ("Entro a CHECK qf: %5d  \n",qf);

	for (i=0; i< qf; i++)
	{
printf ("CHECK proceso linea : %5d \n",lne(i));
		memset(b1,0,MSTR);
		strcpy(b1, (*fnp[i]).l );

		f2 = 1;
		
		/*  lineas comentadas no cuentan */
		if (f2)
		{
			if (b1[0]=='C' || b1[0]=='c' || b1[0]=='!')
				f2 = 0;
		}


		/* otros tipos de lineas comentadas no cuentan */
		if (f2)
		{
			j=0;
			f3 = 1;
	
			while ( f3 && j<strlen(b1) )
			{
				if (b1[j]=='!')
					f3 = 0, f2 = 0;
				if (b1[j]!=' ' && b1[j]!='!')
					f3 = 0;
				if (b1[j]==' ')
					j++;
			}
		}


			
		/* largo de la linea ... */
		if (f2)
		{
			if (strlen(b1) < 73)
				f2 = 0;
		}

		if (f2)
		{
			j = 7;

			l1 = strlen(b1);
			f3 = 1;

			while ( f3 && j < strlen(b1) )
			{
				if (b1[j]=='!')
					f3 = 0, l1 = j-1;
				if (b1[j]!='!')
					j++;
			}

			/* reviso si quedan blancos al final */
			for (j=strlen(b1) - 1, f3=1; f3 && j > 0; j--)
				if (b1[j] == ' ')
					b1[j]= 0, l1--;
				else
					f3=0;



			if (l1 < 73)
				f2 = 0;
		}



		/* si la linea no cumplio ninguno de los criterios de linea valida,
		 * entonces check_file falla !
		 */
		if (f2)
		{
			printf ("CHECK_FILE Linea: %6d (%3d) |%s|\n",lne(i),l1,b1);
			f1 = 0;
		}
			

#if 0
		l_pars(i,&q_tk);


		/* armo la linea de nuevo con todos los tokens
		 * solucion por ahora ... dejo los comentarios al final, afuera
		 */
		memset(b2,0,MSTR);
		for (j=0, f5=1; j< q_tk; j++)
			strcat (b2,tk[j]);
#endif
	}

	return (f1);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_alo
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	cambia sentencias con allocate                            
 *      agrega check_alloc
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */



int	cfor_alo(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,k1,l;
	int	f1,f2,f3,f4,f5;
	int	pf,uf,qf;
	int	m1,m2;
	int	l1,l2;
	int	p1;
	int	t1;

	int	fl_n;
	int	f_proceso;

	char	exte_name[MAXV];
	int	tipo_ext;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	
	char	d1[MAXR];
	char	d2[MAXR];
	char	d3[MAXR];




	memset (b3,'X',MAXB);

	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(907);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif

	strcpy(d3,e_name(fl_n) );
	tipo_ext = 0;
	if (!strncmp(d3,"for",3))
		tipo_ext = 1;
	if (!strncmp(d3,"f90",3))
		tipo_ext = 2;
	
	printf ("WWW1: |%s| \n",(*tb[fl_n]).n );
	printf ("WWW2: |%s| \n",d3);


	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b0, (*fnp[ j ]).l );
		strcpy (b1, pasar_a_minusc(b0));
		strcpy (b2, pasar_a_minusc(b0));

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		l2 = strlen(b1);

		/*
		 * detectar si la linea es comentarios o si esta vacia ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		f_proceso = 1;
		if (linea_vacia(b1) || es_linea_comentario(b1))
			f_proceso = 0;


		/* tipo de caso a manejar */
		t1 = 0;

		/* solo proceso lineas que no son comentario */
		if (f_proceso)
		{	
			if (tiene_allocate(b1))
			{
				/* determino tipo de caso ... */


				/*
				 * caso 1
				 * Es fuente .for 
				 * el allocate esta solo y entero en la linea 
				 *
				 * caso 2
				 * Es fuente .for 
				 * el allocate esta solo y continua en linea siguiente 
				 *
				 * caso 5
				 * Es fuente .f90 
				 * el allocate esta solo y entero en la linea 
				 *
				 */

				if (tipo_ext == 1)
				{
printf ("UUU1 b1       |%s| \n",b1);
printf ("UUU2 tipo_ext |%d| \n",tipo_ext);

					if (es_alloc_t1(b1))
					{
						t1 = 1;
printf ("UUU3 t1       |%d| \n",t1);
					}
					else
					{
						t1 = 2;
					}

printf ("UUU4 t1       |%d| \n",t1);
				}

				if (tipo_ext == 2)
				{
					if (es_alloc_t1(b1))
					{
						t1 = 5;
					}

				}

				printf ("XXX1: (%d,%d) |%s| \n",tipo_ext,t1,b1);


			}


#if 0
			f5 = 1;
			while ( f5 )
			{
				strcpy(b2,b1);

				f5 = 0;
				if (p1 = es_cadena_int_src3(b1,&m1))
				{	f5 = 1;

					memset(d1,0,MAXR);
					strncpy(d1,b1+p1,m1);

		printf ("XXX1: |%s| \n",d1);
						memset (d2,0,MAXR);
						strcpy(d2,d1);
						d2[0] = '=';
						d2[m1-1] = ' ';
						sq_vinit_simple++;
		printf ("XXX2: |%s| \n",d2);


					strncpy(b1+p1,d2,m1);


					sprintf (b4,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b2 );
					sprintf (b5,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b1 );
					
					if (gp_fverbose("d3"))
					{
						printf ("TTTX\n");
						printf ("TTTX%s",b4);
						printf ("TTTX%s",b5);
						printf ("TTTX\n");
					}

#if 1
					if (fflog)
					{
						fprintf (hflog,"%s",b4);
						fprintf (hflog,"%s",b5);
						fprintf (hflog,"%s","\n");
					}
#endif
				}
			}
#endif
		}	

		/* grabo la linea */
		if ( 1 )
		{
			strcpy ((*fnp[ j ]).l, b1 );
		}
	}



#if 0
	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
#endif

}




int	es_alloc_t1(s)
char	*s;
{
	char	b0[MAXB];

	int	f_res;
	int	f_sigo;

	int	p1,p2;
	int	i,j,k;
	int	l1,l2;

	int	f1;

	int	st1,st2,st3,st4;

	char	z[MAXV];
	int	w;
	sprintf (z,"es_alloc_t1");
	w = g_dbg(z);

	/* copio string para trabajar local */
	strcpy(b0,s);
	l2 = strlen(b0);

	/* recorto si hay comentarios al final de la linea */
	for (i=l2-1, f_sigo=1; f_sigo && i; i--)
	{	if (b0[i] == '!' && b0[i-1] != '4')
			f_sigo = 0, b0[i] = 0;
	}

	/* largo definitivo de la sentencia */
	l2 = strlen(b0);
	

	f_res = 0;
	p1    = 0;
	p2    = l2 -1;

	st1 = 0;
	st2 = 0;
	st3 = 0;

		
	/* la primer palabra tiene que ser allocate */
	st1 = 0;
	for (i=0, f_sigo=1 ; f_sigo && i<=p2; i++)
	{	if (b0[i] == ' ' || b0[i] == '\t')
			p1++;
		else
		{	
			if (!strncmp(b0+i,"allocate",8))
			{
				f_sigo = 0;
				st1 = 1;
				p1 = i+8;
			}
		}
	}
	
if (gp_debug && w)
{
mprintf (z,"paso 1 - rimer palabra es allocate          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}


	/* puede haber blancos entre allocate y ( ... los quito */
	while (b0[p1] == ' ')
	{
		l2 = strlen(b0)-1;
		for (k=p1; k<l2; k++)
			b0[k] = b0[k+1];
		b0[l2]=0;
		p2--;
	}


if (gp_debug && w)
{
mprintf (z,"paso 2 - si hay blancos corro la linea          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
}

	/* el ultimo caracter salvo blancos, es un parentesis que cierra */
	st2 = 0;
	l2 = strlen(b0);
	for (i=l2-1, f_sigo=1; f_sigo && i; i-- )
		if (b0[i] == ' ')
			p2--;
		else
		{
			if (b0[i] == ')')
			{
				f_sigo = 0;
				st2 = 1;
				p2  = i;
			}
			else
			{
				f_sigo = 0;
			}
		}

if (gp_debug && w)
{
mprintf (z,"paso 3 - el ultimo caracter salvo blanco es )           \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


	/* es_alloc_t1 : caracteres especificos de allocate entre ( y ) */
	st3 = 1;
	for (i=p1; i<=p2; i++)
	{
		if (!((b0[i] >= 'a' && b0[i] <= 'z') || 
		      (b0[i] >= '0' && b0[i] <= '9') || 
		      b0[i] == ' ' || b0[i] == ':' || b0[i] == ',' || 
		      b0[i] == '-' || b0[i] == '(' || b0[i] == ')' || 
		      b0[i] == '_' || b0[i] == '*' || b0[i] == '+'    )  )
		{
			st3 = 0;
		}
	}

	
if (gp_debug && w)
{
mprintf (z,"paso 4 - caracteres especificos entre ( y )          \n");
mprintf (z,"s           |%s| \n",s);
mprintf (z,"b0          |%s| \n",b0);
mprintf (z,"st1 st2 st3 |%d,%d,%d| \n",st1,st2,st3);
mprintf (z,"p1 p2       |%2d,%2d|  \n",p1,p2);
mprintf (z,"b0[p1]      |%c| \n",b0[p1]);
mprintf (z,"b0[p2]      |%c| \n",b0[p2]);
}


#if 0
	/* tiene que abrir un parentesis */
	st3 = 0;
	if (b0[p1] == '(' )
		st2 = 1;
#endif


	if (st1 && st2 && st3)
		f_res = 1;
			
if (gp_debug && w)
{
mprintf (z,"paso 5           \n");
mprintf (z,"f_res      |%d| \n",f_res);
}


	return (f_res);

}


/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_ini
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	cambia el tipo de inicializacion de var /xxx/ a var = xxx
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */



int	cfor_ini(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,k1,l;
	int	f1,f2,f3,f4,f5;
	int	pf,uf,qf;
	int	m1,m2;
	int	l1;
	int	p1;

	int	fl_n;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	
	char	d1[MAXR];
	char	d2[MAXR];




	memset (b3,'X',MAXB);

	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(907);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif



	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b1, (*fnp[ j ]).l );
		strcpy (b0, (*fnp[ j ]).l );

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		/*
		 * detectar si la linea es comentarios ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		f4 = 1;
		if (es_linea_comentario(b1))
			f4 = 0;

		/* solo proceso lineas que no son comentario */
		if (f4)
		{	
			f5 = 1;
			while ( f5 )
			{
				strcpy(b2,b1);

				f5 = 0;
				if (p1 = es_cadena_int_src3(b1,&m1))
				{	f5 = 1;

					memset(d1,0,MAXR);
					strncpy(d1,b1+p1,m1);

		printf ("XXX1: |%s| \n",d1);
						memset (d2,0,MAXR);
						strcpy(d2,d1);
						d2[0] = '=';
						d2[m1-1] = ' ';
						sq_vinit_simple++;
		printf ("XXX2: |%s| \n",d2);


					strncpy(b1+p1,d2,m1);


					sprintf (b4,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b2 );
					sprintf (b5,"f:%-30.30s l:%06d c:%4d|%s| \n", (*tb[fl_n]).n,lne(j),sq_vinit_simple,b1 );
					
					if (gp_fverbose("d3"))
					{
						printf ("TTTX\n");
						printf ("TTTX%s",b4);
						printf ("TTTX%s",b5);
						printf ("TTTX\n");
					}

#if 1
					if (fflog)
					{
						fprintf (hflog,"%s",b4);
						fprintf (hflog,"%s",b5);
						fprintf (hflog,"%s","\n");
					}
#endif
				}
			}
		}	

		/* grabo la linea */
		if ( 1 )
		{
			strcpy ((*fnp[ j ]).l, b1 );
		}
	}



#if 0
	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
#endif

}





/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_vcb
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	lee una lista de variables con blancos intermedios         
 *	busca en el fuente si encuentra algna de esa variables
 *	si encuentra, las cmbia
 *	ql_f vuelve con nueva ultima linea
 *
 */



int	cfor_vcb(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,k1,k2,l;
	int	f1,f2,f3,f4,f5;
	int	flag;
	int	pf,uf,qf;
	int	m1,m2;
	int	p1;
	int	l1,l2;
	int	c1;

	int	fl_n;
	int	f_sig;

	char	b0[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];
	
	char	d1[MAXR];
	char	d2[MAXR];




	memset (b3,'X',MAXB);

	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(907);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif


	/* cargo archivo con lista de variables vcb */
	if (!ffin2)
		gp_uso(1001);
	
	q_vcb = 0;

	while (fgets(b1,MAXV,hfin2) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}

			/* saco el ultimo | */
			if (b1[strlen(b1)-1] == '|')
				b1[strlen(b1)-1] = 0;
			else
				error(5003);

			/* copio desde el primer caracter despues del primer | */
			strcpy(vcb[q_vcb],b1+1);

printf ("XXX cargue vcb |%s| \n",vcb[q_vcb]);


			q_vcb++;
		}
	}

	if (gp_fverbose("d1"))
	{
		printf ("Cantidad de lineas cargadas : %6d\n",q_vcb);
	}




	/* reviso cada linea */
	for (j = pf; j <= uf; j++)
	{
		/* la linea j */
		strcpy (b0, (*fnp[ j ]).l );
		strcpy (b1, (*fnp[ j ]).l );
		strcpy (b2, pasar_a_minusc(b1));
		l2 = strlen(b2);

		/* parseo fila a tokens ... si tiene sentido  */
		l_pars(j,&q_tk);


		/*
		 * detectar si la linea es comentarios ...
		 * falta mas ... chequear si no puso ! o c en otra posicion que no sea 0
		 */

		c1 = 0;

		f4 = 1;
		if (es_linea_comentario(b2) || linea_vacia_for(b2) )
			f4 = 0;

		/* solo proceso lineas que tienen codigo */
		if (f4)
		{	
			f5 = 1;
			while (f5)
			{
				f5 = 0;
	
				/* veo si encuentro alguna variable ! */
				for (k=0; k< q_vcb; k++)
				{
					strcpy(b3,pasar_a_minusc(vcb[k]));
	
					for (k1=0; k1 < l2; k1++)
					{
						if (!strncmp(b2+k1,b3,strlen(b3)))
						{
							c1++;
							f5 = 1;
	
							/* cambiazos */
							strcpy(b4,b3);
							strcpy(b5,vcb[k]);
							for (k2 = 0; k2 < strlen(b4); k2++)
							{
								if( b4[k2] == ' ')
								{
									b4[k2] = '_';
									b5[k2] = '_';
								}
							}
	
							strncpy(b1+k1,b5,strlen(b5));
							strncpy(b2+k1,b4,strlen(b4));
	
							printf ("ZZ1 b3 var (%5d) |%s|\n",k,b3);
							printf ("ZZ2 b2 lin (%5d) |%s|\n",lne(j),b2);
							printf ("ZZ3 cnt (%5d)\n",c1);
							printf ("ZZ4 b0             |%s|\n",b0);
							printf ("ZZ5 b1             |%s|\n",b1);
							printf ("ZZ6 b2             |%s|\n",b2);
							printf ("ZZ7 \n");

							if (fflog)
							{
								fprintf (hflog,"%s\n",b0);
								fprintf (hflog,"%s\n",b1);
								fprintf (hflog,"%s","\n");
							}
						}
					}
				}		
	
			} /* while f5 */
		}	

		/* grabo la linea */
		if ( 1 )
		{
			strcpy ((*fnp[ j ]).l, b1 );
		}

	}


#if 0
	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
#endif

}










/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_mas
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	f1
 *	1 - integer
 *	2 - logical
 * 	3 - real
 *	4 - character
 *
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */


#if 1

int	cfor_mas(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,i1,j,k,k1;
	int	p1,p2;
	int	f1,f2,f3,f4,f5,f6,f7;
	int	qi,qf;
	int	d1;
	int	delta1,delta2;

	char	b1[MSTR];
	char	b2[MSTR];
	char	b3[MSTR];
	char	b4[MSTR];
	char	b5[MSTR];
	char	b6[MSTR];

	char	*tok;


	d1 = 0;		/* lineas agregadas ! */
	qi = *ql_i;
	qf = *ql_f;
	f4 = 0;		/* modifico linea con kind o len */





	for (i=0; i< qf; i++)
	{
		f4 = 0;

		memset(b1,0,MSTR);
		strcpy(b1, (*fnp[i]).l );

		l_pars(i,&q_tk);

		if (gp_fverbose("d4"))
		{
			printf ("cfor_mas:  Vengo de l_pars\n");
			printf ("cfor_mas:  Linea %4d #tk %4d |%s|\n",i,q_tk,b1);
			for (j=0; j<q_tk; j++)
			{
				printf ("cfor_mas: TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
			}
		}

		if ( f1=tiene_dec_var1() )
		{
			f4 = 1;

			/* caso int, log, real */
			if (f1 )
			{
				if (gp_fverbose("d3"))
				{	printf ("cfor_mas: fixed !  f1: %d \n",f1);
					printf ("cfor_mas: Linea %4d q_tk %4d lin |%s|\n",i,q_tk,b1);
				}
			}
				
		} /* tiene_dec_var */



		/* armo la linea de nuevo con todos los tokens
		 * solucion por ahora ... dejo los comentarios al final, afuera
		 */
		memset(b2,0,MSTR);
		for (j=0, f5=1; j< q_tk; j++)
			strcat (b2,tk[j]);

		/* la linea no tenia dec de variables */
		if ( f4 == 0)
		{
			strcpy ( (*fnp[i]).l, b2);
		}

		/* la linea tenia dec de variables */
		if (f4 == 1)
		{
			if ( def_var_continua(b2))
			{
				if (gp_fverbose("d3"))
				{
					printf ("DEF_var_cont desp del if ... b2: |%s| \n",b2);
				}

				memset (b3,0,MSTR);

				delta1 = 0;
				do
				{
					strcat (b3, limpiar_mas ( (*fnp[i+delta1]).l )  );
					delta1++;
		
					sq_lineas_con_mas_elim++;

					do
					{
						f7=0;
	
						/* 
						 * ojo !! para una misma linea no puede ser true mas de una condicion !! 
						 * si me encuentro una linea que es coment ... skip 
						 */
	
						if ( (*fnp[i+delta1]).l[0] == '!')
							f7=1;
	
						/* 
						 * si me encuentro una linea en blanco  ... skip 
						 */

						if ( (*fnp[i+delta1]).l[0] != '!' && strlen( (*fnp[i+delta1]).l ) < 4 )
							f7=1;
	
						if (f7)
							delta1++;

					} while (f7);

				}
				while ( def_var_continua (  (*fnp[i+delta1]).l) ) ;

				if (gp_fverbose("d3"))
				{
					printf ("limpiar_mas (desp del while) : |%s| \n", limpiar_mas ( (*fnp[i+delta1]).l) );
				}

				strcat (b3, limpiar_mas ( (*fnp[i+delta1]).l) );


				if (gp_fverbose("d3"))
				{
					printf ("\n\n");
					printf ("Concateno1: delta1: %2d len: %2d b3: |%s| \n\n\n",delta1,strlen(b3),b3); 
				}

				/* 
				 * mala eleccion strtok ... hay que canbiar , x & en los lugares que no hay que cortar !!
				 * si hay coma entre esta estructura, reemplazar .. (:,:)
				 */
				for (i1=0; i1<strlen(b3); i1++)
					if (!strncmp(b3+i1,":,:",3) )
						b3[i1+1] = '&';

				for (i1=0,f6=0; i1<strlen(b3); i1++)
				{
					if (!f6 && b3[i1] == '(')
						f6 = 1;
					if ( f6 && b3[i1] == ',')
						b3[i1] = '&';
					if ( f6 && b3[i1] == ')')
						f6 = 0;
				}



				if (gp_fverbose("d3"))
				{
					printf ("\n\n");
					printf ("Concateno2: delta1: %2d len: %2d b3: |%s| \n\n\n",delta1,strlen(b3),b3); 
				}
				
				memset (b4,0,MSTR);
				memset (b5,0,MSTR);

				preparame_dos_lineas(b3,b4,b5);
				strcpy(b6,b5);

				if (gp_fverbose("d3"))
				{	printf ("Vengo de preparame_dos_lineas \n");
					printf ("l1 |%s| \n",b4);
					printf ("l2 |%s| \n",b5);
					printf ("l3 |%s| \n",b6);
				}

				delta2 = 0;
				tok = strtok(b5,",");


				if (gp_fverbose("d3"))
				{
					printf ("CFMAS 5 : 1er strtok  |%s|  \n",tok);
				}

				while ( tok != NULL )
				{
					sprintf (b3,"      %s  %s",b4,tok); 

					tok = strtok(NULL,",");

					if (gp_fverbose("d3"))
					{
						printf ("strtok: |%s| \n",b3);
					}
					delta2++;

				}
				

				if (gp_fverbose("d3"))
				{
					printf ("ZZZ delta1                   :   %4d        \n",delta1);
					printf ("ZZZ delta2                   :   %4d        \n",delta2);
					printf ("ZZZ qf qf-1  s(qf-1)         :   %4d %4d |%s|  \n", qf, qf-1, (*fnp[qf-1]).l );
					printf ("ZZZ i   s(i)                 :   %4d  |%s|  \n", i , (*fnp[i]).l );
					printf ("ZZZ Debo correr lineas desde :   %4d  |%s|  \n",
								 (qf-1)+(delta2-delta1-1) , (*fnp[(qf-1)+(delta2-delta1-1)]).l );
					printf ("ZZZ Hasta linea              :   %4d  |%s|  \n", i+delta2 , (*fnp[i+delta2]).l );
					printf ("\n\n");
				}

				/* correr todas las lineas ... */
				if (gp_fverbose("d3"))
				{
				printf ("MUEVO: ant de for,  delta1   : %2d \n", delta1);
				printf ("MUEVO: ant de for,  delta2   : %2d \n", delta2);
				printf ("MUEVO: ant de for,  i        : %2d \n", i);
				printf ("MUEVO: ant de for,  desde    : %2d \n", (qf-1)+(delta2-delta1-1) );
				printf ("MUEVO: ant de for,  hasta    : %2d \n", i+delta2);
				}


				for ( k = (qf-1)+(delta2-delta1-1); k >= i + delta2  ; k-- )
				{
 
					if ( k >= qf )
					{
						/* agrego lineas al final */ 
						fnp[k] = (fnptr  ) malloc (sizeof (node));
						if ( fnp[k] == NULL)
							error(910);
						(*fnp[k]).l[0] = 0;
						(*fnp[k]).f1 = 0;
						(*fnp[k]).f2 = 0;
						(*fnp[k]).f3 = 0;
					}

					if (gp_fverbose("d3"))
					{
						printf ("KKK2:   a k: %4d   desde: %4d  f(k): |%s|    \n\n",
							k, k-(delta2-delta1-1), (*fnp[k-(delta2-delta1-1)]).l );
					}

					memcpy ( fnp[k],fnp[k-(delta2-delta1-1)], sizeof (node) );
				}


				/* grabar ambas lineas */
				delta2 = 0;
				tok = strtok(b6,",");
				while ( tok != NULL )
				{	
					k1 = i+delta2;

					/* hay que agregar malloc ... deberia conf rango de lineas ! */
					if (fnp[k1] == NULL)
					{				
						fnp[k1] = (fnptr  ) malloc (sizeof (node));
						if ( fnp[k1] == NULL)
							error(912);
						(*fnp[k1]).l[0] = 0;
						(*fnp[k1]).f1 = 0;
						(*fnp[k1]).f2 = 0;
						(*fnp[k1]).f3 = 0;
					}

					sprintf (b3,"      %s  %s",b4,tok);

					if (gp_fverbose("d3"))
					{
						printf ("KK3: GRABO: qf     : %4d  \n", qf);
						printf ("KK3: GRABO: f(x)   : %4d  \n", i+delta2);
						printf ("KK3: GRABO: str de : |%s| \n", b3 );
						printf ("KK3: GRABO: str a  : |%s| \n", (*fnp[i+delta2]).l );
					}


					/* recuperando la chanchada */
					for (i1=0; i1<strlen(b3); i1++)
						if (b3[i1] == '&' )
							b3[i1] = ',';
						
#if 0
						if (!strncmp(b3+i1,":&:",3) )
							b3[i1+1] = ',';
#endif


					strcpy ( (*fnp[i+delta2]).l, b3);

					if (gp_fverbose("d3"))
					{
						printf ("KK4: GRABO: f(x) : %4d  \n", i+delta2);
						printf ("KK4: GRABO: str  :|%s|  \n",  (*fnp[i+delta2]).l );
					}

					tok = strtok(NULL,",");
					delta2++;
				}

			
				/* agregar una linea mas al count */
				qf = qf + delta2 - delta1 - 1;

			}
			else
			{	/* no es linea con continuacion */
				strcpy ( (*fnp[i]).l, b2);
			}
		}
	}

	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
}


#endif


/*
 * recibe linea con continuacion (+) y declaracion de variables 
 * si hay comentarios al final, elimina
 *
 */

char	*limpiar_mas(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	p1;

	static char	b1[MAXB];

	for (i=0, f1=1, p1=0; f1 && i<strlen(s); i++)
		if (s[i] != ' ' && s[i] != '+')
			p1 = i, f1 = 0;

#if 0
	/* por ahora .. si hay comentarios al final, fuera */
	for (i=strlen(s)-1, f1=1; f1 && i; i--)
		if (s[i] == ',')
			s[i+1] = 0, f1 = 0;
#endif

	/* por ahora .. si hay comentarios al final, fuera */
	for (i=strlen(s)-1, f1=1; f1 && i; i--)
		if (s[i] == '!')
			s[i] = 0, f1 = 0;

	strcpy(b1,s+p1);

	return b1;

}



int	def_var_continua(s)
char	*s;
{
	int	i,j,k;
	int	f1,f2,f3;
	int	p1,p2;

	
	f1 = 1;		/* seguimos verificando */
	f2 = 0;		/* default, no tiene char de continua */
	f3 = 0;		/* la linea no tiene comentario al final */

	if (gp_fverbose("d3"))
	{
		printf ("def_var_continua - - - - 1 |%s| \n",s);
	}

	
	p1 = strlen(s);

	/* busco el ultimoo caracter valido en la linea, descartando que haya comentario */
	for (i=0 , f1=1; i < p1; i++)
		if (s[i] == '!')
		{	f3 = 1;
			/* codigo,codigo[,][   ]! texto */
			for (j=i-1; f1 && j; j--)
				if (s[j] != ' ')
					f1=0, p1 = j;
		}

	if (!f3)
		p1--;
			
	if (gp_fverbose("d3"))
	{
		printf ("def_var_continua - - - - 2 strlen(s) : %d p1: %d f2: %d s(p1): %c  s(p1-1): %c |%s| \n",
			strlen(s),p1,f2,s[p1],s[p1-1],s);
	}

	if (s[p1] == ',')
		f1 = 0, f2 = 1;

#if 0
	for ( i=0, f3 = 0; f1 && !f3 && i< strlen(s); i++)
		if (s[i] == '!' )
			p1 = i, f1 = 0, f3 = 1;

	for ( i=p1; f1 && i; i--)
		if (s[i] == ',')
			f2 = 1, f1 = 0;
#endif


	if (gp_fverbose("d3"))
	{
		printf ("def_var_continua - - - - 3 |%s| f2 (ret): (%d) \n\n",s,f2);
	}

	return f2;
}

/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_vars
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	f1
 *	1 - integer
 *	2 - logical
 * 	3 - real
 *	4 - character
 *
 *	Si hay que desdoblar lineas, ql_f vuelve con nueva ultima linea
 *
 */


int	cfor_vars(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k,l1;
	int	p1,p2;
	int	f1,f2,f3,f4,f5;
	int	qi,qf;
	int	d1;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];


	d1 = 0;		/* lineas agregadas ! */
	qi = *ql_i;
	qf = *ql_f;
	f4 = 0;		/* modifico linea con kind o len */



	for (i=0; i< qf; i++)
	{
		f4 = 0;

		strcpy(b1, (*fnp[i]).l );

		l_pars(i,&q_tk);

		if ( f1=tiene_dec_var1() )
		{
			f4 = 1;

			/* caso int, log, real */
			if (f1 == 1 || f1 == 2 || f1 == 3)
			{
				/* si no lo pude arreglar, encontre un caso no contemplado !!*/
				if (! fix_dec_var1 () )
				{	
printf ("DDD despues de fix_dec_var1 q_tk: %d\n",q_tk);

					printf ("CASO NO CONTEMPLADO: (%5d) len: %2d |%s| \n",i,strlen(b1),b1);
				}
#if 0
					error(701);
#endif
				else
				{
					if (gp_fverbose("d3"))
					{	printf ("fixed %d ! \ncfor:  linea %4d #tk %4d |%s|\n",f1,i,q_tk,b1);
						for (j=0; j<q_tk; j++)
						{
							printf ("TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
						}
					}
				}
			}

			/* caso character */
			if (f1 == 4 )
			{
				/* si no lo pude arreglar, encontre un caso no contemplado !!*/
				if (! fix_dec_var2 () )
					error(702);
				else
				{
					if (gp_fverbose("d3"))
					{	printf ("fixed %d! \ncfor:  linea %4d #tk %4d |%s|\n",f1,i,q_tk,b1);
						for (j=0; j<q_tk; j++)
						{
							printf ("TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
						}
					}
				}
			}
				
		} /* tiene_dec_var */



		if (gp_fverbose("d4"))
		{
			printf ("cfor:  linea %4d #tk %4d |%s|\n",i,q_tk,b1);
			for (j=0; j<q_tk; j++)
			{
				printf ("TK: %3d %3d f1: %d |%s|\n",j,strlen(tk[j]),f1,tk[j]);
			}
		}

		/* armo la linea de nuevo con todos los tokens */
		memset (b2,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b2,tk[j]);


		/* la linea no tenia dec de variables */
		if ( f4 == 0)
		{
			strcpy ( (*fnp[i]).l, b2);
		}


		/* la linea tenia dec de variables */
		if (f4 == 1)
		{
			/* en el caso de fix format ... si tiene mas de 72 chars ... problemas */
			if ( (l1=largo_linea(b2)) > LF77 )
			{
				if (gp_fverbose("d3"))
				{	
					printf ("ACA algo no anda carajo !!!! \n");
					printf ("Atencion! -- linea larga %5d %3d |%s|\n",i,l1,b2);
				}
printf (" ZZZ--- 1 \n");

				memset (b3,0,MAXB);
				memset (b4,0,MAXB);

printf (" ZZZ--- 2 \n");

				armame_dos_lineas(b2,b3,b4);
				if (gp_fverbose("d3"))
				{	printf ("dos lineas: 1  |%s| \n",b3);
					printf ("dos lineas: 2  |%s| \n",b4);
				}

printf (" ZZZ--- 3 \n");
				/* correr todas las lineas ... */
				correme_una_linea(i+1,qf);

printf (" ZZZ--- 4 \n");

#if 1
				/* grabar ambas lineas */
				strcpy ( (*fnp[i]).l, b3);
				strcpy ( (*fnp[i+1]).l, b4);
			
				/* agregar una linea mas al count */
				qf++;
#endif

#if 0
				strcpy ( (*fnp[i]).l, b2);
#endif
			}
			else
			{	/* no es linea larga, guardar una sola linea*/
				strcpy ( (*fnp[i]).l, b2);
			}
		}
	}

	/* actualizo la cantidad de lineas del source para proximos cambios */
	*ql_f = qf;
}






/*
 *	correr todas las lineas en el vector
 *	desde la l1 al final
 *	hay que agregar una al final para hacer lugar 
 */

int	correme_una_linea(l1,ql)
int	l1;
int	ql;
{

	int	i,j,k;


	/* agrego una linea al final */
	fnp[ql] = (fnptr  ) malloc (sizeof (node));
	if ( fnp[ql] == NULL)
		error(913);

	(*fnp[ql]).l[0] = 0;
	(*fnp[ql]).f1 = 0;
	(*fnp[ql]).f2 = 0;
	(*fnp[ql]).f3 = 0;


	for (j=ql; j>l1; j--)
	{
		memcpy ( fnp[j],fnp[j-1], sizeof (node) );
	}
}





#if 0

/*	NO ANDA - dejo aca para revisar en otro momento ....
 *	correr todas las lineas en el vector
 *	desde la l1 al final
 *	hay que agregar una al final para hacer lugar 
 */

int	correme_una_linea(l1,ql)
int	l1;
int	ql;
{

	int	i,j,k;



	for (j=ql; j>l1; j--)
	{

		/* agrego una linea al final */
		fnp[j] = (fnptr  ) malloc (sizeof (node));
		if ( fnp[j] == NULL)
			error(914);

		(*fnp[j]).l[0] = 0;
		(*fnp[j]).f1 = 0;
		(*fnp[j]).f2 = 0;
		(*fnp[j]).f3 = 0;

printf (" - - - - 3 l1: %4d ql: %4d j-1: %4d j: %4d  s[j-1]: |%s|  s[j]: |%s| \n", l1,ql,j-1,j, (*fnp[j-1]).l,(*fnp[j]).l );

		memcpy ( fnp[j],fnp[j-1], sizeof (node) );

printf (" - - - - 4 l1: %4d ql: %4d j-1: %4d j: %4d  s[j-1]: |%s|  s[j]: |%s| \n\n\n", l1,ql,j-1,j, (*fnp[j-1]).l,(*fnp[j]).l );

	}
}

#endif

/*
 * -----------------------------------------------------------------------------------
 *
 *	armame dos lineas
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	s  ... linea de entrada a separar 
 *	
 *	separa en dos lineas l1 y l2 
 *	algunos considerandos !
 *
 *	si es del tipo x ej
 *	integer (kind=4) :: variable(subindice1,subindice2) ....
 *	entonces, la ',' esa no sirve!! tengo que estar seguro
 *	que cerro el parentesis !!
 *
 *	idem si es x ej
 *	character (len=40000) :: variable_super_largar / cosa ,
 *	esa ',' tampoco me sirve, tiene que cerrar la '/'
 *
 *	si es una fuction ... tampoco sirve buscar el ::
 */

int	armame_dos_lineas(s,l1,l2)
char	*s;
char	*l1;
char	*l2;
{
	int	i,j,k;
	int	c1;
	int	f1,f2,f3,f4,f5;
	int	p1,p2,p3,p4,p5;
	int	f_fix;
	char	b1[MAXB];

	memset(b1,' ',MAXB);

	f_fix = 0;		/* f_fix 0, aun no se soluciono */

	/* es una FUNCTION ? */
	f5=0;
	p4=0;
	for (i=0; !f5 && i<strlen(s); i++)
		if (!strncmp(s+i,"FUNCTION",8) || !strncmp(s+i,"function",8) )
			p4=i, f5 = 1;


	/* busco el :: - como estoy convirtiendo ... la linea ya lo tiene que tener ! */
	for (i=0, f1=1; f1 && i<strlen(s); i++)
		if (s[i] == ':' && s[i+1] == ':' )
			f1 = 0, p1 = i+2, p2 = i+2;

		
	/* busco donde empieza el resto, despues de :: y blancos */
	for (i=0, c1=0, f1=1; f1 && i<strlen(s); i++)
		if (s[i+p1] == ' ')
			c1++;
		else
			f1=0;

	/* busco una coma que separe campos, que no este dentro de parentesis ! */
	for (i=0, f1=1, f2=1, f3=1, f4=0; f1 && i<strlen(s); i++)
	{	if (f2 && s[i+p1] == '(')
			f2 = 0;
		if (!f2 && s[i+p1] == ')' )
			f2 = 1;

		if (f3 && s[i+p1] == '/')
			f3 = 0, f1=1;
		if (!f3 && s[i+p1] == '/' )
			f3 = 1;

		if (f2 && f3 && s[i+p1] == ',')
			f1 = 0, p2 = p1+i;
	}


	/* si es funcion, separo en declaracion y funcion */
	if (f5)
	{
		p1 = p4 - 1;
		p2 = LF77+1;
	}


	/* 
	 * si encontro la ',' puede separar campos
	 * si no la encontro, es el caso de una sola variable con
	 * sub indices largos , y llego al fin de linea !
	 * caso en que  hay:
	 *     ... (   ,   ) , 
	 *     ... /       /,
	 */
	

	if (p1 != p2)
	{	
printf ("p1 != p2: f4: %d l1: |%s| \n",f4,l1);
printf ("p1 != p2: f4: %d l2: |%s| \n",f4,l2);

		if (p2 > LF77 )
		{

			strncpy(l1,s,p1);
			strcpy(l2,"     + ");
			strcat(l2,s+p1);
			f_fix = 1;
printf ("p2 > LF77: f4: %d l1: |%s| \n",f4,l1);
printf ("p2 > LF77: f4: %d l2: |%s| \n",f4,l2);
		}
		else
		{
			strncpy(l1,s,p2);
			l1[p2]=0;

			strncpy(l2,s,p1);
			strncat(l2,b1,c1);
			strcat(l2,s+p2+1);

			f_fix = 1;
printf ("p2 > LF77,else: f4: %d l1: |%s| \n",f4,l1);
printf ("p2 > LF77,else: f4: %d l2: |%s| \n",f4,l2);
		}

	}
	else
	{
		strncpy(l1,s,p1);
		strcpy(l2,"      + ");
		strcat(l2,s+p1);

		f_fix = 1;
printf ("p1 != p2,else: f4: %d l1: |%s| \n",f4,l1);
printf ("p1 != p2,else: f4: %d l2: |%s| \n",f4,l2);
	}


printf ("salida: f4: %d l1: |%s| \n",f4,l1);
printf ("salida: f4: %d l2: |%s| \n",f4,l2);

#if 0
	if ( p1 == p2)
	{
		printf ("SALGO de armame_dos_lineas \n");
		printf ("l1: |%s| \n",l1);
		printf ("l2: |%s| \n",l2);
	}
#endif


	sq_lineas_desdobladas++;	

}





int	preparame_dos_lineas(s,l1,l2)
char	*s;
char	*l1;
char	*l2;
{
	int	i,j,k;
	int	c1;
	int	f1,f2,f3;
	int	p1,p2,p3;


	if (gp_fverbose("d3"))
	{
		printf ("- preparame s  |%s| \n",s);
	}

	for (i=0, f1=1, p1=0; f1 && i<strlen(s); i++)
		if (s[i] == ':' && s[i+1] == ':' )
			f1 = 0, p1 = i+2;

	for (i=p1, f1=1, p2=0; f1 && i<strlen(s); i++)
		if (s[i] != ' ')
			f1 = 0, p2 = i;

	strncpy(l1,s,p1);
	l1[p1]=0;

	strcpy(l2,s+p2);

	if (gp_fverbose("d3"))
	{
		printf ("- preparame l1 |%s| \n",l1);
		printf ("- preparame l2 |%s| \n",l2);
	}

}


/*
 * -----------------------------------------------------------------------------------
 *
 *	fix_fec_var1
 *
 * -----------------------------------------------------------------------------------
 */



int	fix_dec_var1()
{
	int	i,j,k,i1,l,m,n;
	int	f1,f2,f3,f4;
	int	minus, aster, kind, opt, inten;
	int	dbl, alloca, save, func, cont;
	int	n_minus, n_aster, n_kind, n_opt, n_inten;
	int	n_dbl, n_alloca, n_save, n_func, n_cont;
	int	coma,n_coma,f_coma;
	int	ult, n_type;
	int	f_aster;
	char	nr[16];
	char	b1[MAXB];

	int	f_int,f_rea,f_log,f_cha;

	char	ca;
	char	s_varb[MAXB];
	char	s_inte[MAXB];

	char	b4[MAXB];


	memset(b1,0,MAXB);
	memset(s_varb,0,MAXB);
	memset(s_inte,0,MAXB);


	f3     = 1;	/* f3 true es GO ... si por alguna condicion paramos, f3 false */

	func     = 0;
	inten    = 0;
	save     = 0;
	kind     = 0;
	ult      = 0;
	func     = 0;
	opt      = 0;
	cont     = 0;
	alloca   = 0;
	dbl      = 0;
	aster    = 0;
	coma     = 0;

	n_alloca = 0;
	n_dbl    = 0;
	n_type   = 0;
	n_aster  = 0;
	n_coma   = 0;
	
	f_aster  = 1;
	f_coma   = 1;

	f_int    = 0;
	f_rea    = 0;
	f_log    = 0;
	f_cha    = 0;


	/*
	 * el concepto de ult
	 * es que es la ultima posicion valida del sector
	 * de declaracion de una variable 
	 *
	 */

	for (i=0; i< q_tk; i++)
	{	
		if (!f_int && !strcmp("integer",tk[i]) )
			f_int = 1, minus = 1, n_type = i;

		if (!f_int && !strcmp("INTEGER",tk[i]) )
			f_int = 1, minus = 0, n_type = i;

		if (!f_log && !strcmp("logical",tk[i]) )
			f_log = 1, minus = 1, n_type = i;

		if (!f_log && !strcmp("LOGICAL",tk[i]) )
			f_log = 1, minus = 0, n_type = i;

		if (!f_rea && !strcmp("real",tk[i]) )
			f_rea = 1, minus = 1, n_type = i;

		if (!f_rea && !strcmp("REAL",tk[i]) )
			f_rea = 1, minus = 0, n_type = i;

		if (!dbl && !strcmp(":",tk[i]) && !strcmp(":",tk[i+1]) )
			dbl=1,n_dbl = i;

		if (!f_coma && tk[i][0] == ',')
			f_coma = 1, coma = 1, n_coma = i;

		if (!kind && ( !strcmp("kind",tk[i]) || !strcmp("KIND",tk[i+1]) ))
		{	kind=1,n_kind = i;
			for (k=i+1; k< i+8; k++)
				if (tk[k][0] == ')')
					ult=k;

		}

		if (!strcmp("*",tk[i]) && ( i - n_type < 4 ) )
			aster = 1, n_aster=i;

		if (!strcmp("optional",tk[i]) || !strcmp("OPTIONAL",tk[i]) )
			opt = 1, n_opt = i, ult=i;

		if (!inten && (!strcmp("intent",tk[i]) || !strcmp("INTENT",tk[i]) ))
		{	inten = 1, n_inten = i, ult=i;
			for (k=i+1; k< i+8; k++)
				if (tk[k][0] == ')')
					ult=k;
		}

		if (!strcmp("allocatable",pasar_a_minusc(tk[i])) )
			alloca = 1, n_alloca = i, ult=i;

		if (!strcmp("save",pasar_a_minusc(tk[i])) )
			save = 1, n_save = i, ult=i;

		if (!strcmp("function",pasar_a_minusc(tk[i])) )
		{	func = 1, n_func = i;
		}

		if (!strcmp("&",tk[i]) )
			cont = 1, n_cont = i;
	}



	strcpy(s_varb,tk[n_type]);
	if (minus)
		strcpy(s_varb,pasar_a_minusc(tk[n_type]));


	if (inten)
	{
		/* condiciones para que este todo bien */
		f4=1;
		if (f4 && strcmp(tk[n_inten+1],"("))
			f4 = 0;
		if (f4 && strcmp(tk[n_inten+3],")"))
			f4 = 0;
		if (f4 && 
			( strcmp("in",pasar_a_minusc(tk[n_inten+2]))  &&
			  strcmp("out",pasar_a_minusc(tk[n_inten+2])) &&
			  strcmp("inout",pasar_a_minusc(tk[n_inten+2]))  )
		   )
			f4 = 0;

		if (f4)
		{
			sprintf (s_inte,"%s%s%s%s",tk[n_inten],tk[n_inten+1],tk[n_inten+2],tk[n_inten+3]);
			printf ("fix_dec_var1: INTE: |%s|\n",s_inte);
		}
		else
			error(1001);
	}
			

		
	/* algunos pifies -  casos excepcionales ... kind=4)variable .. */
	if (kind)
	{	

		for (f2=1, k=n_kind+1; k< n_kind+8; k++)
			if (tk[k][0] == ')' && tk[k+1][0] != ' ')
			{
				q_tk++;
				for (l=q_tk-1; l>= k+1 && l; l--)
				{	strcpy(tk[l],tk[l-1]);
				}
				strcpy (tk[k+1]," ");	
				ult = k;
				f2 = 0;
			}
	}


#if 0
no anda ... hay que mejorar esto del ultimo ...

	/* si hay coma despues de lo ultimo valido, es el caso  ... (kind=2), var ... */
	if (coma)
	{	if (n_coma > ult)
			tk[n_coma][0] = ' ';
	}
#endif 

#if 1
	/* algunos casos excepcionales ... logical* 4 var */
	if (aster)
	{	
		strcpy(nr,tk[n_aster+1]);
		if (!es_num_tk(nr) && tk[n_aster+1][0]==' ' && es_num_tk(tk[n_aster+2]) )
		{	strcpy(nr,tk[n_aster+2]);
			strcpy(tk[n_aster+1],nr);
			strcpy(tk[n_aster+2]," ");
		}
	}
#endif



	if (gp_fverbose("d4"))
	{
		printf ("RR1 n_type:   %d \n",n_type);
		printf ("RR1 n_inten:  %d \n",n_inten);
		printf ("RR1   inten:  %d \n",  inten);
		printf ("RR1   aster:  %d \n",  aster);
		printf ("RR1    kind:  %d \n",   kind);

		for ( i1 = 0 ; i1< 20 && i1 < q_tk; i1++)
			printf ("Token-1-  %d |%s| \n",i1,tk[i1]);
	}


	i = n_type;
	f1=0;


	/* caso 1
	 * el formato es correcto, no tocamos nada
	 *
	 * type (kind= nn ), intent(rr) :: pepe
	 * type (kind= nn )  function      pepe
	 * type                         :: pepe
	 * type              function      pepe 
	 *	
	 * tiene kind y dbl  o  tiene kind inten y dbl
	 *
	 */

	if ( !f1 && ((kind && dbl) || (kind && dbl && inten))  )
	{
		f1 = 1;
	}

	if ( !f1 && kind && func )
	{
		f1 = 1;
	}

	if ( !f1 && !aster && !kind && dbl && !func )
	{
		f1 = 1;
	}
	if ( !f1 && !aster && !kind && !dbl && func )
	{
		f1 = 1;
	}

	

	/* caso 2
	 * formato simple
	 *
	 * type*n pepe
	 *
	 * tiene aster 
	 * no tiene kind
	 * no tiene dbl
	 * no es function
	 *
	 * type pepe 	
	 *
	 * no tiene aster
	 * no tiene kind
	 * no tiene dbl
	 *
	 * type*n :: pepe
	 *
	 * tiene aster
	 * no tiene kind
	 * tiene dbl
	 * no es funcion
	 */

	if ( !f1 && aster && !kind && !dbl && !func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1002);

		sprintf (tk[i] , "%s (kind=%c) :: ",s_varb,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		f1 = 1;
	}


	if ( !f1 && !kind && !dbl && !func )
	{

		sprintf (tk[i] , "%s :: ",s_varb);
		
		f1 = 1;
	}


#if 1
	if ( !f1 && aster && !kind && dbl && !func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1003);

		sprintf (tk[i] , "%s (kind=%c) ",s_varb,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		f1 = 1;
	}

#endif


	/* caso 3
	 * formato simple
	 *
	 * type*n :: pepe
	 *
	 * tiene aster 
	 * no tiene kind
	 * tiene dbl
	 * no es function
	 *
	 * type (kind=n) pepe
	 *
	 * no tiene aster
	 * tiene kind
	 * no tiene dbl
	 * no es funcion
	 *
	 */

	if ( !f1 && !aster && kind && !dbl && !func )
	{
		ca = tk[n_kind+4][0];
		if (ca != ' ' )
			error(1004);

		sprintf (tk[n_kind+4],"%s"," :: ");
		
		f1 = 1;

	}





	/* caso 4
	 * formato simple funcion
	 *
	 * type*n function pepe
	 *
	 * tiene aster
	 * no tiene kind
	 * no tiene dbl
	 * es function
	 */

	if ( !f1 && aster && !kind && !dbl && func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1005);

		sprintf (tk[i] , "%s (kind=%c)  ",s_varb,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		f1 = 1;
	}


	/* caso 4
	 * formato con intent 
	 *
	 * type*n, intent pepe
	 *
	 * tiene aster
	 * no tiene kind
	 * no tiene dbl
	 * tiene intent
	 * no es function
	 */

	if ( !f1 && aster && !kind && !dbl && inten && !func )
	{
		ca = tk[n_aster+1][0];
		if (ca != '1' && ca != '2' && ca != '4' && ca != '8' )
			error(1006);

		sprintf (tk[i] , "%s (kind=%c), %s :: ",s_varb,s_inte,ca);
		tk[i+1][0]=0;
		tk[i+2][0]=0;
		
		tk[n_inten+0][0] = 0;
		tk[n_inten+1][0] = 0;
		tk[n_inten+2][0] = 0;
		tk[n_inten+3][0] = 0;
		f1 = 1;
	}


	/* caso especial solo para  TRANSOBJ2.FOR 
	 *
	 * real (kind=4),
	 * se ve que sacaron el allocatable,
	 * quedo la coma, y lahey compila igual 
	 */

#if 1
	if ( !f1 && !aster && kind && !dbl && !inten && !alloca && !func )
	{
printf ("fix_dec_var1: FFF entro caso transobj2 \n");
printf ("EE1|%s| %c \n",tk[n_type],tk[n_kind+5][0]);
		/* caso especifico */
		if (!strcmp(tk[n_type],"REAL") && tk[n_kind+5][0]==','  )
		{
printf ("EE2|%s| %c \n",tk[n_type],tk[n_kind+5][0]);
			sprintf (tk[n_kind+5] , "%s"," :: ");
			f1 = 1;
		}

	}

#endif

	if (gp_fverbose("d4"))
	{
		for ( i1 = 0 ; i1< 15 && i1 < q_tk; i1++)
			printf ("fix_dec_var1: Token:  %d |%s| \n",i1,tk[i1]);
		printf ("\n");
		memset(b1,0,MAXB);
		for (m=0; m<q_tk; m++)
			strcat(b1,tk[m]);
		printf ("fix_dev_var1: Linea: |%s| \n\n",b1);
	}


	/* f1 1, resolvio f1 0, no pudo cambiar formato !! */
	if (f1)
	{	if (f_int)
			sq_integer++;
		if (f_rea)
			sq_real++;
		if (f_log)
			sq_logical++;
	}
	else
	{
		sq_variables_no_convertidas++;

	}


	return(f1);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	fix_dec_var2
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * arregla declaracion de variables cuando son character
 */

int	fix_dec_var2()
{
	int	f2,f3;
	int	i,j,k,l,i1;
	int	minus, aster, len, opt, inten, alloca, save, func, cont, dbl;
	int	n_minus, n_aster, n_kind, n_opt, n_len;
	int	n_inten, n_alloca, n_save, n_func, n_cont, n_dbl;
	int	ult, n_type;
	int	f_aster,f_len;
	int	f_cha;

	char	ca;
	char	nr[16];
	char	s_varb[MAXB];
	char	b1[MAXB];
	char	b2[MAXB];

	int	f1;	/* resolvio si no */

	memset(s_varb,0,MAXB);
	memset(b1,0,MAXB);


	alloca  = 0;
	func    = 0;
	inten   = 0;
	aster   = 0;
	save    = 0;
	len     = 0;
	dbl     = 0;
	ult     = 0;
	func    = 0;
	opt     = 0;
	cont    = 0;
	n_len   = 0;
	n_type  = 0;
	n_aster = 0;
	n_dbl   = 0;


	f_aster = 0;
	f_len   = 0;

	memset(b2,0,MAXB);
	for (j=0; j<q_tk; j++)
		strcat(b2,tk[j]);


	for (i=0; i< q_tk; i++)
	{	
		if (!strcmp("character",tk[i]) )
			f_cha=1,minus = 1, n_type = i;

		if (!strcmp("CHARACTER",tk[i]) )
			f_cha=1,minus = 0, n_type = i;

		if (!dbl && !strcmp(":",tk[i]) && !strcmp(":",tk[i+1]) )
			dbl=1,n_dbl = i;

		if (!f_aster && !strcmp("*",tk[i]) && ( i - n_type < 4 ) )
			f_aster= 1, aster = 1, n_aster=i;

		if (!f_len && !strcmp("len",tk[i]) || !strcmp("LEN",tk[i]) )
		{	f_len=1, len = 1, n_len = i, ult=i;
			for (k=i+1; k<i+8; k++)
				if (tk[k][0] == ')')
					ult=k;
		}

		if (!strcmp("optional",tk[i]) || !strcmp("OPTIONAL",tk[i]) )
			opt = 1, n_opt = i, ult=i;

		if (!strcmp("intent",tk[i]) || !strcmp("INTENT",tk[i]) )
		{	inten = 1, n_inten = i, ult=i;
			for (k=i+1; k< i+8; k++)
				if (tk[k][0] == ')')
					ult=k;
		}

		if (!strcmp("allocatable",tk[i]) )
			alloca = 1, n_alloca = i, ult=i;

		if (!strcmp("save",tk[i]) )
			save = 1, n_save = i, ult=i;

		if (!strcmp("function",tk[i]) )
		{	func = 1, n_func = i;
		}

		if (!strcmp("&",tk[i]) )
			cont = 1, n_cont = i;
	}


	strcpy(s_varb,tk[n_type]);
	if (minus)
		strcpy(s_varb,pasar_a_minusc(tk[n_type]));

		
	/* algunos casos excepcionales ...  len=4)varible ... */
	if (len)
	{	
printf ("ZZZfix_dec_var2: arregle el len=4)var ... \n");

		for (f2=1, k=n_len+1; k< n_len+8; k++)
			if (tk[k][0] == ')' && tk[k+1][0] != ' ')
			{
				q_tk++;
				for (l=q_tk-1; l>= k+1 && l; l--)
				{	strcpy(tk[l],tk[l-1]);
				}
				strcpy (tk[k+1]," ");	
				ult = k;
				f2 = 0;
			}
	}


#if 0
	/* algunos casos excepcionales ... character* 6 var */
	if (aster)
	{	
printf ("ZZZfix_dec_var2:1 estoy en if blanco despues de aster ... |%s| \n",b2);
		if (tk[n_aster+1][0] == ' ')
		{
printf ("ZZZfix_dec_var2:2 estoy en if blanco despues de aster ... |%s| \n",b2);

			strcpy(nr,tk[n_aster+2]);
			if (es_num_tk(nr))
			{
				strcpy(tk[n_aster+1],tk[n_aster+2]);
				strcpy(tk[n_aster+2]," ");
				n_aster--;
			}
			else
				error(2001);
		}
	}

#endif

#if 0
	if (aster)
	{	
		strcpy(nr,tk[n_aster+1]);
		if (!es_num_tk(nr))
		{
			if (tk[n_aster+1][0] != '(' ) 
				error(2001);
		}
	}
#endif

#if 1
	/* algunos casos excepcionales ... character* 6 var */
	if (aster)
	{	
		strcpy(nr,tk[n_aster+1]);
		if (!es_num_tk(nr) && tk[n_aster+1][0]==' ' && es_num_tk(tk[n_aster+2]) )
		{	strcpy(nr,tk[n_aster+2]);
			strcpy(tk[n_aster+1],nr);
			strcpy(tk[n_aster+2]," ");
		}
	}
#endif


	if (gp_fverbose("d3"))
	{
		for ( i1 = 0 ; i1< 15 && i1<q_tk ; i1++)
			printf ("Token-1-  %d |%s| \n",i1,tk[i1]);
	}

	i = n_type;
	f1=0;



 
	/* caso 0 - esta bien formado, no hay que hacer nada !
	 * tiene len
	 * tiene dbl 
	 *
	 * character (len=10) :: pepe
	 * character (len=*)  :: pepe
	 */

	if ( !f1 && (len && dbl) || (len && !dbl &&func)  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 0 ... |%s| \n",b2);

		f1 = 1;
	}

	/* caso 1
	 * no tiene asterisco
	 * no tiene numero
	 * no tiene dbl
	 * no tiene len
	 * no es funcion 
	 * character esta solo en la linea (variables siguen en otra linea)
	 *
	 * character 
	 */

	if ( !f1 && !aster && !dbl && !len && !func  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 1 ... |%s| \n",b2);

		if (ult == 0)
			ult = i+1;

		if (ult == q_tk)
			q_tk++;

		sprintf (tk[ult],"%s"," :: ");
		f1 = 1;
	}


	/* caso 2 
	 * tiene asteristco
	 * tiene numero
	 * no tiene dbl
	 * no es funcion
	 *
	 * character*32 pepe
	 */
	
	if ( !f1 && aster && es_num_tk(nr) && !dbl && !func  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 2 ... |%s| \n",b2);
		strcpy(nr,tk[n_aster+1]);
		tk[n_aster+1][0] = 0;

		sprintf (tk[i],"%s (len=%s) ",s_varb,nr);
	
		if (ult == 0)
			ult = i+1;

		sprintf (tk[ult],"%s",":: ");

#if 0
		/* casos excepcionales ... la PM con los blanquitos */
		if (tk[ult+1][0]=='*')
			tk[ult+1][0]=' ';
#endif
		f1 = 1;
	}

	/* caso 3 
	 * no tiene asteristco
	 * tiene len
	 * no tiene dbl
	 * no es funcion
	 * character esta solo en la linea (variables siguen en otra linea)
	 */

	if ( !f1 && !aster && !dbl && len && !func  )
	{
printf ("ZZZfix_dec_var2: estoy en caso 3 ... |%s| \n",b2);

		if (ult == 0)
			ult = i+1;

		if (ult != 0)
			ult++;

		if (ult == q_tk)
			q_tk++;

		sprintf (tk[ult],"%s"," :: ");
		f1 = 1;
	}


	/* caso 4 
	 * tiene asteristco
	 * tiene numero
	 * tiene dbl
	 * no es funcion
	 *
	 * character*32 :: pepe
	 */

	if (!f1 && aster && es_num_tk(tk[n_aster+1]) && dbl && !func )
	{
		strcpy(nr,tk[n_aster+1]);
		tk[n_aster+0][0] = 0;
		tk[n_aster+1][0] = 0;

		sprintf (tk[i],"%s (len=%s)",s_varb,nr);
	
		if (ult == 0)
			ult = i+1;

		f1 = 1;

	}

	
	/* caso 5 
	 * tiene asteristco
	 * tiene (*)
	 * no tiene dbl
	 * no es funcion
	 */
	
	if (!f1 && aster && !es_num_tk(tk[n_aster+1]) && !dbl && !func )
	{
		if (ult == 0)
		{	k=0;
			while (tk[n_aster+k][0] != ')')
				k++;
			ult = n_aster+k+1;

			if (ult == q_tk)
				q_tk++; 
		}

		sprintf (tk[n_aster+0],"%s"," ");
		sprintf (tk[n_aster+2],"%s","len=*");
		sprintf (tk[n_aster+4],"%s"," :: ");
		f1 = 1;
	}

	/* caso 6 
	 * tiene asteristco
	 * tiene (*)
	 * tiene dbl
	 * no es funcion
	 */
	
	if (!f1 && aster && !es_num_tk(tk[n_aster+1]) && dbl && !func )
	{
		if (ult == 0)
		{	k=0;
			while (tk[n_aster+k][0] != ')')
				k++;
			ult = n_aster+k+1;

			if (ult == q_tk)
				q_tk++; 
		}

		sprintf (tk[n_aster+0],"%s"," ");
		sprintf (tk[n_aster+2],"%s","len=*");
		sprintf (tk[n_aster+4],"%s"," ");
		f1 = 1;
	}







	/* no pudo hacer fix ! */
	if (!f1)
	{
		memset(b2,0,MAXB);
		for (j=0; j<q_tk; j++)
			strcat(b2,tk[j]);

		printf ("No hubo caso para .... |%s| \n",b2);

		if (gp_fverbose("d3"))
		{
			printf ("FIX_dev_var2: caso: X f1: %d |%s| \n",f1,b2);
		}
	}




	/* f1 1, resolvio f1 0, no pudo cambiar formato !! */
	if (f1)
	{	if (f_cha)
			sq_character++;
	}
	else
	{
		sq_variables_no_convertidas++;

	}


	return(f1);
}

















int	tiene_dec_var1()
{
	int	i,j;
	int	f1,f2,f3;
	char	b1[MAXV];

	int	f_comm, n_comm;

	f1=0;	/* f1 0 indica que no encontro declaracion de variable */
	
#if 0
	if (tk[0][0] == '!')
		f1 = 1;
#endif

	f_comm	= 0;
	n_comm  = 0;
	f2      = 1;
	memset (b1,0,MAXV);


	for (i=0; !f1 && f2 && i<q_tk; i++)
	{
		if ( !strcmp("integer", pasar_a_minusc( tk[i] )) )
			f1 = 1;
		if ( !strcmp("logical", pasar_a_minusc( tk[i] )) )
			f1 = 2;
		if ( !strcmp("real", pasar_a_minusc( tk[i] )) )
			f1 = 3;
		if ( !strcmp("character", pasar_a_minusc( tk[i] )) )
			f1 = 4;

		/* vuelvo a armar la linea a partir de los tokens */
		strcat(b1,tk[i]);

		/* condiciones para terminar de buscar */
		if (!strcmp("&",tk[i]))
			f2 = 0;
		if (!strcmp("!",tk[i]))
			f2 = 0;
		if (strlen(b1) > 12)
			f2 = 0;
			
	}

	return f1;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	largo_linea
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * devuelve el largo de linea real de una sentencia fortran
 * sin tener en cuenta si puso un chorro de comentarios al final
 *
 */


int	largo_linea(s)
char	*s;
{
	int	i,j,k;
	int	f1;


	k=0;
	for (i=0,f1=1; f1; i++)
		if (s[i] == 0 || ( i> 10 && s[i] == '!') )
			f1=0, k = i;

printf ("LLL: calculado  %3d |%s| \n",k,s);
printf ("LLL: strlen     %3d |%s| \n",strlen(s),s);

	return k;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	linea_vacia_for 
 *
 *	edicion especial para sources fortran
 *
 *	Determina si una linea esta vacia (generalmente, para lineas leidas de files)
 *	La linea NO esta vacia si:
 *
 *	1) contiene al menos 1 caracter distinto de
 *	' ' 	blanco
 *	'\t' 	tab
 *	'\n'	new line
 *
 *	2) si el primer caracter es distinto de '!'
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	linea_vacia_for(s)
char	*s;
{
	int f1;
	int i,flag;

	f1 = 1;
	i=0;
	flag=1;

	/* si toda la linea es comentario, consideramos vacia para fortran */
	if (f1 && s[0] == '!')
		f1=0;

	for (i=0; f1 && flag && i< strlen(s); i++)
		if (s[i] != ' ' && s[i] != '\t' && s[i] != '\n' )
			flag=0 , f1=0;

		
	return flag;
}














/*
 * -----------------------------------------------------------------------------------
 *
 *	tool5
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	Descrip
 *
 * 
 *
 */


int	pro_tool5()
{

	int	px,py;

	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	tk[MAXT][MAXB];

	char	z[MAXV];
	sprintf (z,"tool5");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout || !ffaux)
		gp_uso(117);

/* tool5 */
#if 1

	/* si encuentro caracteres no considerados para el parser, avisar al final de todo el proceso */
	flag_caracteres = 0;

	q_lin=0;
	memset(b1,32,MAXB);
	memset(b2,0,MAXB);

	while (fgets(b1, MAXB, hfinp) != NULL)
	{
	    /* proceso todas las lineas */
	    if ( 1 )
	    {
#if 1
		/* blancos al final */
		for (i=strlen(b1)-1, f4=1 ; i && f4 ; i-- )
			if (b1[i] == ' ' || b1[i] == '\n' || b1[i] == '\r' )
				b1[i]=0;
			else
				b1[i+1]='\n', b1[i+2]=0, f4=0;
#endif
	
		f1=1;

		if (gp_fverbose("d3"))
		{
			printf ("Linea  : %d \n\n",q_lin);
			printf ("Buffer :|%s|\n",  b1);
		}


		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			/* controlamos cantidad de tokens ... */
			if (q_tk > MAXT-10)
			{	error(503);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
							
					}
					p1++; 
					break; 

				/* letras */
				case TC_LET:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_LET || \
						(j == TC_NUM && !char_demed(b1[p1-1])  ) )
						tk[q_tk][p2++]=b1[p1++];
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* numeros tenemos que contemplar 3.3 o 3,3 !! */
				case TC_NUM:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_NUM || \
						(tipo_char(b1[p1]) == TC_PNT && tipo_char(b1[p1+1]) == TC_NUM ) || \
						( (b1[p1]) == ',' && tipo_char(b1[p1+1]) == TC_NUM ) )
					{	tk[q_tk][p2]=b1[p1];
						p1++;
						p2++;
					}
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* choto */
				case TC_CHO:
					/* opcion: no los guardo */
					tk[q_tk][0]=' ';
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

#if 0
					while ( (j=tipo_char(b1[p1])) == TC_CHO)
					{	printf ("estoy perdido en choto %d \n",p1);
						p1++;	
					}

#endif
					break;


				/* blanco o tab */
				case TC_BLA:
#if 0
					/* opcion: no los guardo */
					while ( (j=tipo_char(b1[p1])) == TC_BLA)
					       p1++;	
#endif
					/* opcion: los guardo */
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;

					break;

				/* coma */
				case TC_CCE:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* punto */
				case TC_PNT:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis abre */
				case TC_PAA:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis cierra */
				case TC_PAC:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				case TC_EOL:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

				/* caracteres varios */
				case TC_CVR:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				default:
					printf ("Default, algo salio mal  !!!\n\n");
					f1=0;
					break;
			}

		} /* while */


		if (gp_fverbose("d3"))
			printf ("termine de parsear linea \n");


		/* verifico si hay que sacar output  en minusculas */
		if (gp_minusculas)
		{
			for (j=0; j< q_tk; j++)
				strcpy(tk[j],pasar_a_minusc(tk[j]));
		}


		/* salida en formato token columnar */
		if (gp_fsentencia == 0)
		{

			/* pidio nivel de descripcion en salida ... agrego la sentencia */
			if (gp_niveldes)
				fprintf (hfout,"%s\n",b1);

			/* grabo los tokens encontrados */
			for (j=0; j< q_tk; j++)
			{
				switch (gp_niveldes)
				{

					case 0:
						fprintf (hfout,"%s\n",tk[j]);
						break;

					case 1:
						fprintf (hfout,"%3d,%s\n",j,tk[j]);
						break;

					default:
						fprintf (hfout,"%s\n",tk[j]);
						break;
				}


				if (gp_fverbose("d3"))
					printf ("%3d,%s\n",j,tk[j]);

			}

		}



		/* salida en formato sentencia */
		if (gp_fsentencia == 1)
		{
			/* grabo los tokens encontrados */
			for (j=0; j< q_tk; j++)
			{
				fprintf (hfout,"%s",tk[j]);

				if (gp_fverbose("d3"))
					printf ("%3d,%s\n",j,tk[j]);
			}


			/* se termino la linea */
			fprintf (hfout,"\n");
		}


		for (j=0; j< q_tk; j++)
		{
			strcat (b2,tk[j]);
		}
		strcat (b2,"\n");

		if(gp_fverbose("d3"))
			printf ("strcat: |%s| \n",b2);

		if (gp_fverbose("d3"))
		{
			printf ("\n");
		}



		/* 
		 * Termine todo lo que tenia que hacer con esta linea,
		 * sumo lineas 
		 *
		 */

		memset(b1,32,MAXB);
		memset(b2,0,MAXB);
		q_lin++;


	    } /* if ... no esta vacia la linea */

	}  /* while fgets ... */

	
	if (gp_fverbose("d2"))
	{
		printf ("Cant de lineas procesadas %d\n",q_lin);
		printf ("\n\n\n");
	}



#endif
/* tool5 */



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}




/*
 * -----------------------------------------------------------------------------------
 *
 *	tool6
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	Descrip
 *
 * 	carga un src en memoria, en vector a pnt de esctructs
 *
 *	- cambia lineas de continuacion
 *
 * 
 *	ffinp
 *	ffout   Archivo de salida con cambios realizados
 *	ffaux   Generalmente, errores del parser
 *	ffsta   Estadisticas de los procesos
 *
 *	ffckf	Ir guardando info general de todo el proceso 
 *
 *	Para entradas/salidas extras de los procesos
 *
 *	ffin2
 *	ffou2
 *	fflog
 *
 */


#if 1

int	pro_tool6()
{
	int	i,j,k;
	int	ql_ini,ql_fin;
	int	l1,n_l1;
	int	lml,n_lml;
	int	f1;
	int	pf,uf;

	char	b1[MAXB];

	char	z[MAXV];
	sprintf (z,"tool6");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/*
	 * en aux graba caracteres no contemplados por el parser
	 */

	/* que la vida sea mas leve */
	if (!ffaux)
		agregar_ffaux("parser.err");
	if (!ffsta)
		agregar_ffsta("tool.sta");
	if (!ffckf)
		agregar_ffckf("check.log");
	if (!fflog)
		agregar_fflog("p.log");


	if (!ffinp || !ffout || !ffaux)
		gp_uso(118);



	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&ql_ini);

	/* compatibiliad con la corrida tipo bunch files */
	/* registro datos del archivo */
	qf_ff=0;
	tb[qf_ff] = (ffptr ) malloc (sizeof (ff));
	if ( tb[qf_ff] == NULL )
		error(915);

	strcpy ( (*tb[qf_ff]).n, extract_fname(finp));
	(*tb[qf_ff]).pf = 0;
	(*tb[qf_ff]).uf = ql_ini-1;
	qf_ff++;

	/* proceso file */
	pf = (*tb[0]).pf ;
	uf = (*tb[0]).uf ;

	if (gp_fverbose("d3"))
	{
		for (i=0; i<qf_ff; i++)
		{
			printf ("PRO_TOOL6: file a procesar: |%s| \n", (*tb[i]).n );
		}
	}

	/* registro nombre de archivo en check log */
	if (ffckf)
	{
		fprintf (hfckf,"File: %s\n", (*tb[0]).n);
	}


#if 0
	lml=0;
	for (i=0; i<ql_ini; i++)
	{
		strcpy(b1, (*fnp[i]).l );
		if ( b1[0] != 'C' && b1[0] != 'c' && b1[0] != '!')
		{	
			l1 = strlen(b1);

			for (k=0,f1=1; f1 && k < strlen(b1); k++)
			{
				if (b1[k]=='!')
					f1=0,l1=k-1;
			}
				
			for (k=strlen(b1)-1, f1=1; f1 && k; k--)
				if (b1[k] == ' ')
					b1[k]=0, l1--;
				else
					f1 = 0;

			if ( l1 > lml )
			{	lml = l1, n_l1 = lne(i);
				printf ("LLL |%s|\n",b1);
			}
		}
	}
#endif


	lml_src_sc(&lml,&n_lml,pf,uf);



	if (gp_fverbose("d2"))
	{
		printf ("Linea mas larga: %4d\n\n",lml);
		printf ("|%s|\n",(*fnp[n_lml]).l );
	}


	if (ffckf)
	{
		fprintf (hfckf,"Linea mas larga encontrada antes de procesar file (sin coments)  %3d \n",lml);
		fprintf (hfckf,"Nro de linea: %4d \n",lne(n_lml));
		fprintf (hfckf,"|%s|\n",(*fnp[n_lml]).l );
	}


	/* mientras que no cambie la cant de lineas !!! */
	ql_fin=ql_ini;

	
	/* 0 - pidio cambiar comentarios                              */
	if ( ffchg_com )
		cfor_comm(&ql_ini,&ql_fin);

	/* 1 - pidio borrar comentarios                               */
	if ( ffchg_dec )
		cfor_dec(&ql_ini,&ql_fin);
	
	/* 2 - pidio cambiar type selectors                           */
	if ( ffchg_typ )
		cfor_vars(&ql_ini,&ql_fin);

	/* 3 - pidio sacar las lineas de cont. y replicar declaracion */
	if ( ffchg_mas )
		cfor_mas(&ql_ini,&ql_fin);

	/* 4 - pidio convertir formato var/xx/ por var = xx           */
	if ( ffchg_ini )
		cfor_ini(&ql_ini,&ql_fin);
	
	/* 5 - pidio cambiar lineas de continuacion                   */
	if ( ffchg_lco )
		cfor_lcon(&ql_ini,&ql_fin);

	/* 6 - pidio cambiar variables con blancos intermedios        */
	if ( ffchg_vcb )
		cfor_vcb(&ql_ini,&ql_fin);

	/* 7 - pidio agregar check a lineas con allocate              */
	if ( ffchg_alo )
		cfor_alo(&ql_ini,&ql_fin);
	

	/* hago un checking final de las lineas ! */
	if ( 0 )
	{
		if ( !check_file(&ql_fin) )
		{
			printf ("CHECK LINEAS  largas !! \n");
	
			if (ffckf)
			{
				fprintf (hfckf,"Atencion ! file quedo con filas largas (si es f90 no hacer caso) !! \n");
			}
		}
	}

	/* grabo file */
	for (i=0; i< ql_fin; i++)
	{
		strcpy(b1, (*fnp[i]).l );

		if (gp_minusculas)
			strcpy( b1 , pasar_a_minusc( (*fnp[i]).l )  );

		fprintf (hfout,"%s\n",b1);
	}


	mostrar_reportes("tool6");


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}

#endif




/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_dec
 *
 *	borra todos los comentarios
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * borra comentarios:
 * a) si son lineas de codigo 
 * b) que esten despues de lineas de codigo "activas "
 * 
 * deja:
 * aquellos comentarios que describen algoritmos y demas 
 *
 */


#if 1

int	cfor_dec(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	qi,qf;
	int	fl_n;
	int	pf,uf,ln;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];


	qi = *ql_i;
	qf = *ql_f;
	memset(b4,' ',MAXB);


	/* compatibilidad con cfor_vars / p_src3 etc
	 *
	 * primera y ultima fila del source 
	 * poner nombre del file en table de files
	 */

	pf = 0;
	uf = *ql_i - 1;

	fl_n = 0;

#if 1
	/* registro datos del archivo */
	tb[fl_n] = (ffptr ) malloc (sizeof (ff));
	if ( tb[fl_n] == NULL )
		error(916);

	strcpy ( (*tb[fl_n]).n, extract_fname(fout));
	(*tb[fl_n]).pf = pf; 
	(*tb[fl_n]).uf = uf; 
	(*tb[fl_n]).ql = uf - pf + 1;

#endif



	/* recorro todas las lineas */
	for (i=pf; i<= uf ; i++)
	{
		/* copio linea y linea siguiente */
		strcpy(b1, (*fnp[i]).l );
		strcpy(b2, (*fnp[i]).l );
		strcpy(b3, (*fnp[i]).l );

#if 0
		/* parseo en tokens */
		l_pars(i,&q_tk);

		if(gp_fverbose("d4"))
		{
			printf ("cfor_dec: b1: |%s| \n",b1);
			for (j=0; j<q_tk; j++)
			{
				printf ("cfor_dec: TK: %3d (%3d) |%s|\n",j,strlen(tk[j]),tk[j]);
			}	
			printf ("\n");
		}
#endif

#if 0
		if ( es_linea_comentario(b1))
		{
			if (gp_fverbose("d3"))
			{
				printf ("cfor_dec: com detectado \n");
				printf ("cfor_dec: %5d |%s|\n",lne(i),b1);
			}
		
			strcpy(b2,"!");

			/* actualizo stats */
			sq_comentarios_elim++;
		}
#endif

	/* borro comentarios al final */

		if (gp_cborrar == 1)
		{
			if ( !es_linea_comentario(b3) && tiene_coment_final (b3,&p1))
			{
				b3[p1] ='\0';

				if (fflog)
				{
					fprintf (hflog,"%5d %s\n",lne(i),b1);
					fprintf (hflog,"%5d %s\n",lne(i),b3);
					fprintf (hflog,"%s","\n");
				}

				sq_coment_al_final_elim++;
			}
		}


		/* armo la linea de nuevo con todos los tokens */
#if 0
		memset (b3,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b3,tk[j]);
#endif

		strcpy ( (*fnp[i]).l, b3);


	}
}




#endif


/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_comm
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	devuelve true si la linea es de este tipo 
 *      
 *      !     aca hay cualquier cosa      ! comentario
 *            aca hay cualquier cosa      ! comentario
 *            aca hay cualquier cosa      ! comentario !!! expresivo
 *
 *	no modifica las lineas tipo
 *
 *      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *            aca hay cualquier cosa ' no tocar ! '  
 *
 *	ojo con las lineas que tienen pepe's margin .... ( que HDP !!! )
 *
 *	INTEGER (KIND=2) :: LOCAL_MAX_MONTHS=12,LOCAL_MAX_YEARS=12 ! UP'ED FOR DOUG 051310 ! UP'ED FOR SCOTT 02/26/08 ! UP'ED FOR SCOTT 01/08/04
 *
 */

int	tiene_coment_final(s,p)
char	*s;
int	*p;
{
	int	i,j,k;
	int	l1;
	int	c1,c2,c3,c4;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	f_ret,f_sig;
	char	b1[MAXB];



	/* lo basico .. si no nay ' y encuentro un ! ... es coment */


	f_ret = 0;
	f_sig = 1;

	f1 = 0;
	f4 = 1;
	l1 = strlen(s);
	i = 7;             /* la eleccion de i es crucial ... */

	while (f4 && i < l1 )
	{
		if ( f1 && s[i] == '\'' )
			f1 = 0;

		if (!f1 && s[i] == '\'' )
			f1 = 1;

		if (!f1 && s[i] == '!' )
		{
			f_ret = 1;
			f_sig = 0;
			*p = i;
			f4 = 0;
		}

		i++;
	}
		



	if (f_sig)
	{

	f_ret = 0;
	f_sig = 1;
	i = strlen(s);
	*p = 0;

		
	/* si despues de ! hay un solo ' , todo lo demas no funca */
	c4 = 0;
	f4 = 1;
	j  = 8;
	p2 = 0;

	while ( f4 && j < i)
	{	if (s[j] == '!')
			p2 = j;

		if (p2 && s[j] == '\'')
		{
			c4++;
		}

		j++;
	}

	}


	/* ver si no se mando !!!!!!!!!!!!!!! !! !!! !!!! - hay cada uno */
	if (f_sig)
	{
		c1 = 0;
		c2 = 0;
		c3 = 0;

		for (i=0; i < strlen(s); i++)
		{	if (s[i] == '!')
				c1++;
			if (s[i] == ' ')
				c2++;
			if (s[i] != '!' && s[i] != ' ')
				c3++;
		}

		if (c1 > 15 && c3 < 10)
		{
			f_sig = 0;
			f_ret = 0;
		}
	}

			

	i = strlen(s);
	f1 = 0;
	f2 = 0;

	while (f_sig)
	{
		/* solo si hay mas de un solo '  */
		if (c4 > 1)
		{

		if (s[i] == '\'')
		{	if (f1 == 0)
				f1 = 1;
			else
				f1 = 0;
		}
		}

		
		if (s[i] == '"')
		{	if (f2 == 0)
				f2 = 1;
			else
				f2 = 0;
		}


		if (f1 == 0 && f2 == 0)
		{
			if (s[i] == '!')
			{	f_ret = 1;
				p1 = i;
				*p = i;
			}
		}

		i--;
		if (i <= 6)
			f_sig = 0;
	}

	return (f_ret);
}

















/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_comm
 *
 * -----------------------------------------------------------------------------------
 */


#if 1

int	cfor_comm(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	qi,qf;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];


	qi = *ql_i;
	qf = *ql_f;
	memset(b4,' ',MAXB);

	/* recorro todas las lineas */
	for (i=0; i< qi ; i++)
	{
		/* copio linea y linea siguiente */
		strcpy(b1, (*fnp[i]).l );

		/* truchada ... */
		if (b1[4]=='!')
		{
			b1[4]=' ';
			b1[0]='!';
		}

		l_pars(i,&q_tk);

		if (tk[4][0]=='!')
		{
			tk[4][0]=' ';
			tk[0][0]='!';
		}

		if(gp_fverbose("d4"))
		{
			for (j=0; j<q_tk; j++)
			{
				printf ("TK: %3d %3d |%s|\n",j,strlen(tk[j]),tk[j]);
				if (tk[j][0] == 9)
					printf ("TAB: !!!! en linea %5d |%s| \n",i,b1);
			}
			printf ("voy b1: |%s| \n",b1);
		}

		if ( es_linea_comentario(b1))
		{
			if (gp_fverbose("d3"))
			{
				printf ("cfor: com detectado \n");
				printf ("cfor: %4d #tk %4d |%s|\n",i,q_tk,b1);
			}

			/* pongo comentario
			 * hay casos en que el comentario es 
			 * 'C1000 ..... '
			 * 
			 */
#if 0
			strcpy (tk[0],"!");
#endif
			tk[0][0]='!';

			/* actualizo stats */
			sq_lineas_com++;
		}

		/* armo la linea de nuevo con todos los tokens */
		memset (b3,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b3,tk[j]);

		strcpy ( (*fnp[i]).l, b3);

		if (gp_fverbose("d3"))
			printf ("fix: |%s|\n",b3);

	}
}




#endif


/*
 * -----------------------------------------------------------------------------------
 *
 *	es_linea_comentario
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	recibe una linea completa
 *	revisa condiciones para determinar si
 *	la linea entera es un comentario
 *
 *	detecta casos:
 *	c ....
 *	C ....
 *	! ....
 *             ! ....
 *      *******
 */

int	es_linea_comentario(s)
char	*s;
{

	int	i,j,k;
	int	f1,f2,f3;

	char	b1[MSTR];

	char	z[MAXV];
	int	w;
	sprintf (z,"es_linea_comentario");
	w = g_dbg(z);

	strcpy(b1,s);

if (gp_debug && w)
{
mprintf (z,"b1: |%s| \n",b1);
}

	f1 = 1;		/* sigo verificando */
	f2 = 1;		/* es comentario */
	i  = 0;
	
	
	if ( b1[0] == 'c' || b1[0] == 'C' || b1[0] == '!' )
		f1 = 0;
	else	
		f2 = 0;

	/* caso especial encontrado */
	if ( !strncmp(b1,"***",3))
		f2 = 1, f1 = 0;

	/* hay casos en los que la linea es '    !' */
	while (f1 && i< strlen(b1) )
	{
		if (b1[i] == '!')
			f1 = 0, f2 = 1;

		if ( b1[i] != ' ' && b1[i] != '!' )
			f1 = 0, f2 = 0;

		i++;
	}

if (gp_debug && w)
{
mprintf (z,"b1: |%s| \n",b1);
mprintf (z,"f2: %d   \n",f2);
mprintf (z,"salgo ...\n");
}
	return f2;
}



int	es_linea_comentario2(s)
char	*s;
{

	int	i,j,k;
	int	f1,f2,f3;

	char	b1[MAXB];

	strcpy(b1,s);

	f1 = 1;		/* sigo verificando */
	f2 = 1;		/* es comentario */
	i  = 0;
	

	if ( b1[5] == '+' || b1[6] == '+')
	{
		f1=0;
	}
	else
	{

	if ( b1[0] == 'c' || b1[0] == 'C' || b1[0] == '!' )
		f1 = 0;
	else	
		f2 = 0;


	while (f1 && i< strlen(b1) )
	{
		if (b1[i] == '!')
			f1 = 0, f2 = 1;

		if ( b1[i] != ' ' && b1[i] != '!' )
			f1 = 0, f2 = 0;

		i++;
	}
	}

	return f2;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	cfor_lcon
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	en esta linea falla ! lo pone antes del !
 *
 *	error....   CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',
 *
 */

int	cfor_lcon(ql_i,ql_f)
int	*ql_i;
int	*ql_f;
{

	int	i,j,k;
	int	p1,p2;
	int	f1,f2,f3,f4;
	int	qi,qf;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];
	char	b4[MAXB];
	char	b5[MAXB];


	qi = *ql_i;
	qf = *ql_f;
	memset(b4,' ',MAXB);

	/* ultima linea -1, la ultima linea no puede tener continuacion ... */
	for (i=0; i< qi - 1; i++)
	{
		/* copio linea y linea siguiente */
		strcpy(b1, (*fnp[i]).l );
		strcpy(b2, (*fnp[i+1]).l );

		l_pars(i,&q_tk);

		if(gp_fverbose("d4"))
		{
			for (j=0; j<q_tk; j++)
			{
				printf ("TK: %3d %3d |%s|\n",j,strlen(tk[j]),tk[j]);
			}
			printf ("voy b2: |%s| \n",b2);
		}

		if ( tiene_mas(b2) )
		{
			/* OJO! si tiene mas, pero la linea anterior 
			 * es comentario ... no soportamos ese cambio
			 * parar con error 
			 */

#if 0
			/* si tiene linea comentada con simb cont, le pongo continuacion igual */
			if ( b1[0] == 'C' || b1[0] == 'c' || b1[0] == '!')
			{	printf ("Linea %d tiene comentario y siguiente con + \n",i);
				error(801);
			}
#endif

			if (gp_fverbose("d3"))
			{
				printf ("cfor: mas detectado \n");
				printf ("cfor: %4d #tk %4d |%s|\n",i,q_tk,b1);
				printf ("cfor: %4d     %4d |%s|\n",i+1,0,b2);
			}

			/* tengo que arreglar ambas lineas */
			memset (b3,0,MAXB);
			for (j=0; j< q_tk; j++)
				strcat (b3,tk[j]);

			if (tiene_coment_intermedio (b3) )
			{
				for (j=2, f4=1; f4 && j<q_tk; j++)
				{
					if (!strcmp(tk[j],"!"))
					{
						sprintf (tk[j],"& ! ");
						f4=0;
					}
				}
			}
			else
			{
				/* ponemos el & al final ... */
				memset(tk[q_tk],0,MAXB);

				if (gp_rellena)
				{	
					strncpy(tk[q_tk++],b4,90-strlen(b3));
				}

				sprintf (tk[q_tk++],"   &");
			}


			/* me falta la linea siguiente con el + */
			/* q&d ... no time */
			f1=1;
			for (j=0; f1 && j<strlen(b2); j++)
			{	
				if (b2[j]=='+')
				{		
					b2[j]=' ';
					f1=0;
				}
			}


			sq_lcont++;
		}


		/* armo la linea de nuevo con todos los tokens */
		memset (b3,0,MAXB);
		for (j=0; j< q_tk; j++)
			strcat (b3,tk[j]);
		strcpy ( (*fnp[i]).l, b3);
		if (gp_fverbose("d3"))
			printf ("fix: |%s|\n",b3);

		/* copio la segunda linea, sin el mas */
		strcpy ( (*fnp[i+1]).l, b2);
	}
}



/*
 *	en esta linea falla ! lo pone antes del !
 *
 *	error....   CALL MG_LOCATE_WRITE(15,35,'error reading definition file!',
 *
 */

int	tiene_coment_intermedio (s)
char	*s;
{
	int 	i,j,k;
	int	f1,f2,f3;
	int	c1;

	for (i=0, c1=0, f2=0; i<strlen(s); i++)
		if (s[i] == '\'')
		{
			c1++;
			if ( c1 == 2)
				f2 = 1;
		}


	f1=0;
	for (i=0; i<strlen(s); i++)
		if (s[i] == '!')
			f1 = 1;
	return f1;
}


int	tiene_mas(s)
char	*s;
{
	int	i,j,k;	
	int	f1,f2,f3;
	char	b1[MAXB];

	f1=0;
	f2=1;

	strcpy(b1,s);

	for (i=0; !f1 && f2 && i<strlen(s); i++)
	{
		if (b1[i] != ' ')
		{	if (b1[i] == '+' && b1[i+1] == ' ')
			{
				f1 = 1;
			}
			else
				f2 = 0;
		}
	}

	return f1;
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_amper
 *
 * -----------------------------------------------------------------------------------
 */


int	tiene_amper(s)
char	*s;
{
	int	i,j,k;	
	int	f1,f2,f3;
	char	b1[MSTR];
	int	l2;

	char	z[MAXV];
	int	w;
	sprintf (z,"tiene_amper");
	w = g_dbg(z);

	f1=0;
	f2=1;

	strcpy(b1,s);
	l2 = strlen(b1);

if (gp_debug && w)
{
mprintf (z,"s : |%s|\n",s);
mprintf (z,"b1: |%s|\n",b1);
mprintf (z,"l2: %d\n",l2);
}


	for (i=0; !f1 && f2 && i<l2; i++)
	{
		if (b1[i] == '&')
		{	
			f1 = 1;
			f2 = 0;
		}
	}

if (gp_debug && w)
{
mprintf (z,"s : |%s|\n",s);
mprintf (z,"b1: |%s|\n",b1);
mprintf (z,"l2: %d\n",l2);
mprintf (z,"f1: %d\n",f1);
mprintf (z,"salgo .... \n");

}
	return f1;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool 7 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool7
 *
 *	Devuelve linea mas larga en file
 *	Esto es fundamental para luego saber como procesar
 *	archivos con la opcion --chglcon
 *	(lineas de continuacion )
 *
 */


/* bloque */
#if 1


int	pro_tool7()
{
	int	i,j,k;
	int	f1,f2,f3,f4;
	int	p1;
	char	b1[MAXB];
	char	b2[MAXB];
	FILE	*hwi;

	int	lml;
	int	n_lml;
	
	int	lml2;
	int	n_lml2;
	
	int	ql_ini;



	char	z[MAXV];
	sprintf (z,"tool7");

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp )
		gp_uso(119);

	/* bloque */
	/* cargamos file en memo */
	fnq1 = &fnp[0];
	qfv_load(hfinp,fnq1,&ql_ini);

	lml    = 0;
	n_lml  = 0;
	lml2   = 0;
	n_lml2 = 0;

	for (i=0, f1 = 0; i<ql_ini; i++)
	{
		strcpy(b1,(*fnp[i]).l);


		if ( f4 = tiene_coment_final(b1,&p1))
			b1[p1] = 0;


		if (!es_linea_comentario2 (b1))
		{
			if ( (k = strlen( b1) ) > lml)
				lml = k, n_lml = i;

		}

		if (es_linea_comentario2 (b1) )
		{	

			
			f1++;
			if ( (k = strlen( b1 )) > lml2)
				lml2 = k, n_lml2 = i;
		}

	}

	if (gp_fverbose("d1"))
	{
		
		printf ("\n\n");
		printf ("Cantidad de lineas %7s : %d %s\n"," ",ql_ini," s/com");
		printf ("Linea mas larga   (%5d): %d \n",n_lml+1,lml);
		printf ("|%s|\n\n",(*fnp[n_lml]).l );
		if (f1)
		{
		printf ("Cantidad de lineas %7s : %d %s\n"," ",f1," c/com");
		printf ("Linea mas larga   (%5d): %d \n",n_lml2+1,lml2);
		printf ("|%s|\n\n",(*fnp[n_lml2]).l );
		printf ("\n\n");
		}
			
	
	}


	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */



/* bloque 1 */
#if 1


/*
 * -----------------------------------------------------------------------------------
 *
 *	lml_src_sc 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	Devuelve linea mas larga en file
 *	considera lineas sin comentarios
 *
 */


int	lml_src_sc(ar_lml,ar_n_lml,pf,uf)
int	*ar_lml;
int	*ar_n_lml;
int	pf,uf;
{
	int	i,j,k;
	int	f1,f2,f3,f4;
	int	p1;
	char	b1[MAXB];

	int	lml1;
	int	n_lml1;
	
	int	lml2;
	int	n_lml2;
	

	lml1   = 0;
	n_lml1 = 0;
	lml2   = 0;
	n_lml2 = 0;

	for (i=pf; i<= uf; i++)
	{
		strcpy(b1,(*fnp[i]).l);

		if ( tiene_coment_final(b1,&p1) )
			b1[p1] = 0;


		if (!es_linea_comentario2 (b1))
		{
			if ( (k = strlen( b1) ) > lml1)
				lml1 = k, n_lml1 = i;
		}

		if (es_linea_comentario2 (b1) )
		{	
			if ( (k = strlen( b1 )) > lml2)
				lml2 = k, n_lml2 = i;
		}
	}

	*ar_lml   = lml1;
	*ar_n_lml = n_lml1;

}


#endif
/* bloque 1 */



/*
 * -----------------------------------------------------------------------------------
 *
 *	len_linea_src           
 *
 * -----------------------------------------------------------------------------------
 */


int	len_linea_src(s,tipo,l1,l2)
char	*s;
int	*tipo;
int	*l1,*l2;
{
	int	i,j,k;
	int	f_res;

	f_res = 0;
	
	return (f_res);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool 8 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool8
 *
 *
 *	revisar mthnmcom ...
 *	muestra que variables han quedado con un mix de blanco y _
 */


/* bloque */
#if 1


int	pro_tool8()
{
	int	i,j,k;
	int	f1,f2,f3;
	int	f_sigo,flag;
	int	p1,p2;
	char	b1[MAXB];
	char	b2[MAXB];
	int	c1,c2;
	int	ql;
	FILE	*hwi;

	char	z[MAXV];
	sprintf (z,"tool8");

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!fflog)
		agregar_fflog("vars.log");
	if (!ffinp || !ffout)
		gp_uso(120);



	ql=0;
	while (fgets(b1,MAXB,hfinp) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}
	
			for (i=0, f_sigo=1; f_sigo && i<strlen(b1); i++)
			{
				if (!strncmp(b1+i,"::",2))
				{
					f_sigo=0;
					strcpy(b2,b1+i+3);

					for (j=strlen(b2), f_sigo=1; f_sigo && j >= 7; j--)
					{	
						if (b2[j]=='!')
						{
							b2[j]=0;
							j--;
							while (b2[j]==' ')
								b2[j]=0,j--;

							f_sigo=0;
						}
					}


					/* grabo todas las variables encontradas */
					printf ("var:%3d |%s|\n",ql,b2);
					fprintf (hfout,"|%s|\n",b2);


					/* si la var tiene mix de blancos y _ ... mando a log */
					for (j=0, c1=0, c2=0; j<strlen(b2); j++)
					{
						if (b2[j]==' ')
							c1++;
						if (b2[j]=='_')
							c2++;

					}

					if (c1 && c2)
						fprintf (hflog,"|%s|\n",b2);

				}
			}

			ql++;
		}
	}
	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */



/*
 * -----------------------------------------------------------------------------------
 *
 *	pro_tool 9 
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *
 *	tool9
 *
 */


/* bloque */
#if 1


int	pro_tool9()
{
	int	i,j,k;
	int	f1,f2,f3;
	int	ql;
	int	ptr;
	int	stp;
	int	l1,l2;
	int	c1,c2;
	int	p1,p2;

	char	b0[MSTR];
	char	b1[MSTR];
	char	b2[MSTR];

	char	d0[MAXB];

	char	z[MAXV];
	int	w;
	sprintf (z,"tool9");
	w = g_dbg(z);

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if (!ffinp || !ffout)
		gp_uso(121);


	ql=0;
	while (fgets(b0,MSTR,hfinp) != NULL)
	{
		/* limpiamos el line feed */
		for ( f1=0, j=strlen(b0); !f1 && j >= 0; j--)
			if (b0[j] == '\n')
				b0[j]=0,f1=1;


		/* sacamos el ultimo | */
		strcpy(b1,b0+49);
		for ( f1=0, j=strlen(b1); !f1 && j >= 0; j--)
			if (b1[j] == '|')
				b1[j]=0,f1=1;

		strcpy(b2,b1);


		/* sacamos todos los blancos */
		ptr = 0;
		do
		{
			if (b1[ptr] == ' ')
			{
				l2 = strlen(b1)-1;
				for (k=ptr; k<l2; k++)
				{	b1[k] = b1[k+1];
				}
				b1[l2]=0;
			}
			else
				ptr++;

		} while (b1[ptr]);


		/* sacamos los comentarios al final */
		for ( f1=0, j=strlen(b1)-1 ; !f1 && j > 9 ; j--)
			if (b1[j] == '!')
				b1[j]=0;


		/* sacamos el stat si tiene, guardamos registro de que tiene */
		memset(d0,0,sizeof(d0));
		for ( f1=0, j=strlen(b1)-1 ; !f1 && j > 9; j-- )
			if (!strncmp(b1+j,",stat=",6))
			{	f1=1;
				strcpy(d0,b1+j+1);
				d0[strlen(d0)-1]=0;
				b1[j] = ')';
				b1[j+1] = 0;
			}


		/* intentamos contar variables ... */
		stp = 0;
		for (j=8 , l2=strlen(b1), c1 = 0, c2= 1 ; j < l2;  j++)
		{
			if (b1[j] == '(' )
			{	c1++;
				if (c1 == 1)
					p1 = j+1;
			}
			
			if (b1[j] == ')' )
			{	c1--;
				if (c1 == 1)
				{	p2 = j-1;

					memset(gv_str[stp],0,MAXB);
					strncpy(gv_str[stp],b1+p1,p2-p1+2);
					stp++;
				}
			}

		

			if (b1[j] == ',' && c1 == 1)
			{	c2++;
				p1 = j+1;

			}
		}




		printf ("%04d,|%s|\n",ql,b2);
		printf ("%04d,|%s| - |%s| (%03d) \n",ql,b1,d0,c2);

		for (j=0; j<c2; j++)
			printf ("--%03d |%s|\n",j,gv_str[j]);

		printf ("\n");

		ql++;
	}

	printf ("\nCant de lineas: %d \n",ql);

	/* proceso */
	if (gp_fverbose("d2"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


#endif
/* bloque */





/*
 * -----------------------------------------------------------------------------------
 *
 *	lne (line)
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * para que las lineas que imprimo de los vectores (que empiezan en cero)
 * coincidan con las lineas en los archivos al verlos con vi (que empieza en  1)
 */


int	lne(l)
int	l;
{
	return (l+1);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	A partir de aca ... rutinas generales
 *	No agregar nada particular a un programa determinado (no insistas)
 *
 * -----------------------------------------------------------------------------------
 */


/*
 * -----------------------------------------------------------------------------------
 *
 *	abro_files
 *
 * -----------------------------------------------------------------------------------
 */

int	abro_files()
{

	char	z[MAXV];
	sprintf (z,"abro_files");


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	if ( ffinp && ((hfinp = fopen (finp,"r")) == NULL) )
	{
		error(101);
	}

	if ( ffin2 && ((hfin2 = fopen (fin2,"r")) == NULL) )
	{
		error(102);
	}

	if ( ffin3 && ((hfin3 = fopen (fin3,"r")) == NULL) )
	{
		error(125);
	}

	if ( ffout && ((hfout = fopen (fout,"w")) == NULL) )
	{
		error(103);
	}

	if ( ffou2 && ((hfou2 = fopen (fou2,"w")) == NULL) )
	{
		error(104);
	}

	if ( ffou3 && ((hfou3 = fopen (fou3,"w")) == NULL) )
	{
		error(134);
	}

	if ( ffou4 && ((hfou4 = fopen (fou4,"w")) == NULL) )
	{
		error(135);
	}

	if ( ffou5 && ((hfou5 = fopen (fou5,"w")) == NULL) )
	{
		error(136);
	}

	if ( ffou6 && ((hfou6 = fopen (fou6,"w")) == NULL) )
	{
		error(137);
	}

	if ( ffou7 && ((hfou7 = fopen (fou7,"w")) == NULL) )
	{
		error(138);
	}

	if ( ffaux && ((hfaux = fopen (faux,"w")) == NULL) )
	{
		error(105);
	}
	
	if ( fflog && ((hflog = fopen (flog,"w")) == NULL) )
	{
		error(106);
	}

	if ( ffsta && ((hfsta = fopen (fsta,"w")) == NULL) )
	{
		error(107);
	}

	if ( ffckf && ((hfckf = fopen (fckf,"w")) == NULL) )
	{
		error(108);
	}

	if ( ffdbg && ((hfdbg = fopen (fdbg,"r")) == NULL) )
	{
		error(109);
	}

	if ( ffsrc && ((hfsrc = fopen (fsrc,"r")) == NULL) )
	{
		error(110);
	}

	if ( fflst && ((hflst = fopen (flst,"r")) == NULL) )
	{
		error(111);
	}




	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}



/*
 * -----------------------------------------------------------------------------------
 *
 *	cierro_files
 *
 * -----------------------------------------------------------------------------------
 */


int	cierro_files()
{

	sprintf (gp_p,"cierro_files");
	gp_cnt=0;

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}


	if ( ffinp)
		fclose(hfinp);

	if ( ffin2)
		fclose(hfin2);

	if ( ffin3)
		fclose(hfin3);

	if ( ffout)
		fclose(hfout);

	if ( ffou2)
		fclose(hfou2);

	if ( ffou3)
		fclose(hfou3);

	if ( ffou4)
		fclose(hfou4);

	if ( ffou5)
		fclose(hfou5);

	if ( ffou6)
		fclose(hfou6);

	if ( ffou7)
		fclose(hfou7);

	if ( ffaux)
		fclose(hfaux);

	if ( fflog)
		fclose(hflog);

	if ( ffsta)
		fclose(hfsta);

	if ( ffckf)
		fclose(hfckf);

	if ( ffdbg)
		fclose(hfdbg);

	if ( ffsrc)
		fclose(hfsrc);

	if ( fflst)
		fclose(hflst);



	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}



}



/*
 * -----------------------------------------------------------------------------------
 *
 *	load_file
 *
 *	carga un archivo
 *	formato de vector de punteros a estructuras
 *
 * -----------------------------------------------------------------------------------
 */


int	load_file(fpr)
FILE	*fpr;
{
	char	z[MAXV];
	sprintf (z,"load_file");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}


/*
 *
 *	load_file
 *
 *	carga un archivo sobre lista de nodos encadenados.
 *	no utilizamos esta version ...
 *	hay memoria, y es menos complicado con vector
 *	de punteros a estructuras malloqueadas... 
 *
 */

#if 0

int	load_file(fpr)
FILE	*fpr;
{
	char	b1[MAXB];
	int	flag,f1;
	int	i,j,k;
	int	ql;

	sprintf (gp_p,"load_file");
	gp_cnt=0;


	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}




	/* cantidad de palabras en el diccionario */
	q_wrd = 0;

	/* ptr al contenedor de direccion del ultimo nodo de la cadena */
	npa = (pnodeptr *) &np1;

	ql=0;
	while (fgets(b1,MAXB,fpr) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n')
					b1[j]=0,flag=1;

			if (gp_verbose)
			{
				printf ("%3d |%s|\n",ql,b1);
			}

			*npa = (pnodeptr ) malloc ( sizeof (knode));
			(**npa).wrd = ( char *) malloc(strlen(b1)+1);
			sprintf ( (**npa).wrd,"%s",b1);
			(**npa).num = q_wrd+1;
			(**npa).nx = (pnodeptr) NULL;
			npa = (pnodeptr *) & (*npa)->nx;

			q_wrd++;
			ql++;
		}
	}

	if (gp_verbose("d1"))
	{
		printf ("Cantidad de lineas cargadas : %6d\n",q_wrd);
	}


	if (gp_verbose)
	{
		printf ("\n\nComprobando integridad del diccionario : \n\n");
	
		i=0;
		npa = (pnodeptr *) &np1;

		while ( (*npa) != (pnodeptr) NULL )
		{
			printf ("i: %3d  (**npa).num: %3d  (**npa).wrd: |%s|\n",i,(**npa).num,(**npa).wrd);
			npa = (pnodeptr *) & (*npa)->nx;
			i++;
		}
	}

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[gp_cnt++],gp_p);
	}
}

#endif

















/*
 * -----------------------------------------------------------------------------------
 *
 *	mostrar_cargas
 *
 *	mostrar resultados de cargas, si los hubo
 *
 * -----------------------------------------------------------------------------------
 */

int	mostrar_cargas()
{

	int px,py;

	/* proceso  */
	if (gp_fverbose("d1"))
	{	printf ("%s Entra a mostrar cargas \n\n",gp_tm());
	}

	/* si hubo cargas, las muestro aca */

	/* proceso  */
	if (gp_fverbose("d1"))
	{	printf ("%s Sale de mostrar cargas \n\n",gp_tm());
	}

}





/*
 * -----------------------------------------------------------------------------------
 *
 *	l_pars
 *
 * -----------------------------------------------------------------------------------
 */

/*
 *	line parser
 *	parsea una linea de src en un char ... a un vector de tokens
 *
 *	atenti:
 * 	agregamos para l_pars (que lo usamos mucho para el proyecto fortran)
 *	que cambie tab TC_BLA ... (cuando es 9), por 4 blancos 
 */

/* line parser */
#if 1

int	l_pars(line_number,qt)
int	line_number;
int	*qt;
{

	int	px,py;

	int	i,j,k;
	int	m1,m2,m3;
	int	f1,f2,f3,f4;
	int	q_lin;
	int	q_tk;
	int	p1,p2,p3,p4;

	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];

	char	z[MAXV];
	sprintf (z,"l_pars");

	/* proceso */
	if (gp_fverbose("d3"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}



	/* si encuentro caracteres no considerados para el parser, avisar al final de todo el proceso */
	flag_caracteres = 0;

	q_lin=0;
	memset(b1,32,MAXB);
	memset(b2,0,MAXB);

	strcpy (b1, (*fnp[ line_number ]).l );

	/* si no hay tokens ... devuelvo cant de tokens en 0 */
	q_tk=0;
	*qt=0;

	if (!linea_vacia(b1))
	{

#if 1
		/* blancos al final */
		for (i=strlen(b1)-1, f4=1 ; i && f4 ; i-- )
			if (b1[i] == ' ' || b1[i] == '\n' || b1[i] == '\r' )
				b1[i]=0;
			else
				b1[i+1]='\n', b1[i+2]=0, f4=0;
#endif
	
		f1=1;

		if (gp_fverbose("d3"))
		{
			printf ("l_pars: Linea  : %d \n",line_number);
			printf ("l_pars: Buffer :|%s|\n",  b1);
		}


		/* comienzo parser de tokens */
		p1=0;
		q_tk=0;

		while ( f1 )
		{
			/* controlamos cantidad de tokens ... */
			if (q_tk > MAXT-10)
			{	error(504);
			} 

			j=tipo_char(b1[p1]);

			switch (j)
			{
				/* otro caracter !!! */
				case TC_RST:
					flag_caracteres=1;
					if (gp_fverbose("d1"))
					{
						printf ("Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
						if (ffaux)
							fprintf (hfaux,"Caracter no definido en parser: %03d \n",(unsigned int)b1[p1]);
					}
					p1++; 
					break; 

				/* letras */
				case TC_LET:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_LET || \
						(j == TC_NUM && !char_demed(b1[p1-1])  ) )
						tk[q_tk][p2++]=b1[p1++];
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* numeros tenemos que contemplar 3.3 o 3,3 !! */
				case TC_NUM:
					p2=0;
					while ( (j=tipo_char(b1[p1])) == TC_NUM || \
						(tipo_char(b1[p1]) == TC_PNT && tipo_char(b1[p1+1]) == TC_NUM ) || \
						( (b1[p1]) == ',' && tipo_char(b1[p1+1]) == TC_NUM ) )
					{	tk[q_tk][p2]=b1[p1];
						p1++;
						p2++;
					}
					tk[q_tk][p2]=0;
					q_tk++;
					break;

				/* choto */
				case TC_CHO:
					/* opcion: no los guardo */
					tk[q_tk][0]=' ';
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

#if 0
					while ( (j=tipo_char(b1[p1])) == TC_CHO)
					{	printf ("estoy perdido en choto %d \n",p1);
						p1++;	
					}

#endif
					break;


				/* blanco o tab */
				case TC_BLA:
#if 0
					/* opcion: no los guardo */
					while ( (j=tipo_char(b1[p1])) == TC_BLA)
					       p1++;	
#endif
					/* opcion: los guardo */
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;

					break;

				/* coma */
				case TC_CCE:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* punto */
				case TC_PNT:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis abre */
				case TC_PAA:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* parentesis cierra */
				case TC_PAC:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				case TC_EOL:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					p1++;
					f1=0;
					break;

				/* caracteres varios */
				case TC_CVR:
					tk[q_tk][0]=b1[p1];
					tk[q_tk][1]=0;
					q_tk++;
					p1++;
					break;

				/* fin de linea */
				default:
					printf ("Default, algo salio mal  !!!\n\n");
					f1=0;
					break;
			}

		} /* while */


		if (gp_fverbose("d3"))
			printf ("l_pars: Termine de parsear linea (%d tokens) \n",q_tk);


		/* verifico si hay que sacar output  en minusculas */
		if (gp_minusculas)
		{
			for (j=0; j< q_tk; j++)
				strcpy(tk[j],pasar_a_minusc(tk[j]));
		}




	/* solo para l_pars version proyecto fortran !! - tabs x 4 blancos 
	 * en caso de usar tipo_char para parser
	 * TC_BLA viene definido true si es blanco o tab
	 * y esta seteado en guardar 1 solo x token
	 */

		if (gp_tabs)
		{
			for (j=0; j< q_tk; j++)
			{
				if (tk[j][0] == 9)
					sprintf (tk[j],"%s","    ");
			}
		}

#if 0
		/* verificamos si quedo bien la linea !! */
		for (j=0; j< q_tk; j++)
		{
			strcat (b2,tk[j]);
		}
		strcat (b2,"\n");

		if(gp_fverbose("d3"))
			printf ("strcat: |%s| \n",b2);
#endif


		/* Termine todo lo que tenia que hacer con esta line */
		*qt = q_tk;

	} /* if linea vacia */



	/* proceso */
	if (gp_fverbose("d3"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}


}







#endif
/* line parser */


/*
 * -----------------------------------------------------------------------------------
 *
 *	Rutinas de uso comun
 *
 * -----------------------------------------------------------------------------------
 */


/*
 * -----------------------------------------------------------------------------------
 *
 *	mprintf
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * printf modificado, para que siempre imprima 
 * el primer argumento ... que debe ser el 
 * nombre del modulo que se esta debugeando 
 *
 */

void mprintf(const char *first_arg, const char *fmt, ...) 
{
    va_list args;
    va_start(args, fmt);

    // Imprimir el primer argumento con el formato fijo
    printf("%-20.20s: ", first_arg);

    // Imprimir el resto de los argumentos usando `vprintf`
    vprintf(fmt, args);

    va_end(args);
}




/*
 * -----------------------------------------------------------------------------------
 *
 *	g_dbg
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * revisa si tiene que activar los bloques de debug dentro del modulo 's'
 *
 */

int	g_dbg(s)
char	*s;
{
	int	i,j;
	int	f_res;
	int	f_sig;

	for (i=0, f_res=0, f_sig=1; f_sig && i<q_dbg; i++)
	{
		if (!strcmp(s,tdbg[i]))
			f_res = tflg[i], f_sig = 0;
	}

	return (f_res);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	cargar_debug_def
 *
 * -----------------------------------------------------------------------------------
 */

/*
 * carga lista de nombres de rutinas para marcar como debug si / no 
 * funcion en combinacion con gp_debug ... 
 *
 */

int	cargar_debug_def(ql)
int	*ql;
{
	int	i,j,k;
	int	flag;
	int	qlin;
	char	b1[MAXB];
	char	b2[4];

	char	z[MAXV];
	sprintf (z,"cargar_debug_def");

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[0],z);
	}


	qlin = 0;
	while (fgets(b1,MAXB,hfdbg) != NULL)
	{
		if (!linea_vacia(b1)  && b1[0] != '#' )
		{
			/* saco el fin de linea - contemplo 13 x fuentes fortran */
			for ( flag=0, j=strlen(b1); !flag && j; j--)
				if (b1[j] == '\n' )
				{	
					flag=1;
					if ( j && b1[j-1] == 13)
						b1[j-1]=0;
					else
						b1[j]=0;
				}


			memset(b2,0,sizeof(b2));
			strncpy(b2,b1,1);
			tflg[qlin] = atoi(b2);
			strcpy(tdbg[qlin],b1+2);
			qlin++;
		}
	}

	if (gp_fverbose("d2"))
	{
		printf ("Carga de lista para debug \n");
		for (j=0; j<qlin; j++)
			printf ("%2d %d |%s| \n",j,tflg[j],tdbg[j]);
		printf ("\nTotal de lineas cargadas :  %d \n",qlin);
	}


	*ql = qlin;

	/* proceso */
	if (gp_fverbose("d1"))
	{	printf ("%s%s%s\n\n",gp_tm(),gp_m[1],z);
	}
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	gna
 *
 *	genera numero al azar
 *	numero = rand () % (N-M+1) + M;   // Este está entre M y N
 *
 * -----------------------------------------------------------------------------------
 */


int	gna(min,max)
int	min;
int	max;
{
	int nro;

	nro = rand() % (max-min+1) + min;

	return nro;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gnf
 *
 *	genera numero al azar
 *	numero = rand () % (N-M+1) + M;   // Este está entre M y N
 *
 * -----------------------------------------------------------------------------------
 */


char	*gnf(mes)
int	mes;
{
	static	char	fecha[16];
	int	a,m,d;

	a = 2018;

	m = mes+1;

	if ( m == 2 )		
		d = gna(1,28);

	if ( m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
		d = gna(1,31);

	if ( m == 4 || m == 6 || m == 9 || m == 11 )
		d = gna(1,30);


	sprintf (fecha,"%04d%02d%02d",a,m,d);

	return fecha;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	gnh
 *
 *	genera hora al azar
 *	numero = rand () % (N-M+1) + M;   // Este está entre M y N
 *
 * -----------------------------------------------------------------------------------
 */


char	*gnh(desde,hasta)
int	desde;
int	hasta;
{
	static	char	hora[16];
	int	h,m,s;

	h = gna(desde,hasta);
	m = gna(0,59);
	s = gna(0,59);

	sprintf (hora,"%02d%02d%02d",h,m,s);

	return hora;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	gsf 
 *
 *	fecha en formato mas 'readabily' para imprimir
 *
 * -----------------------------------------------------------------------------------
 */



char	*gsf(f)
char	*f;
{
	static	char	fecha[16];
	int	a,m,d;

	char	s[16];

	
	memset (s,0,16);
	strncpy(s,f,4);
	a = atoi(s);

	memset (s,0,16);
	strncpy(s,f+4,2);
	m = atoi(s);

	memset (s,0,16);
	strncpy(s,f+6,2);
	d = atoi(s);


	sprintf (fecha,"%04d/%02d/%02d",a,m,d);

	return fecha;
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	gsh
 *
 *	Genera string con hora para imprimir
 *
 *
 * -----------------------------------------------------------------------------------
 */


char	*gsh(sh)
char	*sh;
{
	static	char	hora[16];
	int	h,m,s;

	int	h1,m1;
	int	ext;

	char	b[16];

	
	memset (b,0,16);
	strncpy(b,sh,2);
	h = atoi(b);

	memset (b,0,16);
	strncpy(b,sh+2,2);
	m = atoi(b);

	memset (b,0,16);
	strncpy(b,sh+4,2);
	s = atoi(b);

	sprintf (hora,"%02d:%02d:%02d",h,m,s);

	return hora;
}








/*
 * -----------------------------------------------------------------------------------
 *
 *	csv_parser
 *
 *	
 *	parser string tipo csv
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	
 *	fx		funcion a llamar con cada campo procesado
 *
 */


int	csv_parser(str,fx)
char	*str;
int	(*fx)(int,int,char *);
{
	char	b1[MAXB];
	char	b2[MAXB];
	char	b3[MAXB];

	int	p1,p2;
	int	k;
	int	f1;


	strcpy(b1,str);

	k=0;

		
	f1=1;

	if (f1)
	{
		p1=0;
		p2=0;
		k=0;

		while (b1[p1] )
		{	
			b2[k++] = b1[p1];

			if (b1[p1] == ',' || b1[p1] == '\n')
			{	
				b2[k-1]=0;

				strcpy(b3,b2);
				if (gp_fverbose("d4"))
					printf ("=%02d= %s\n",p2,b3);

				fx(0,p2,b3);

				p2++;
				k=0;
			}

			p1++;
		}
		
		/* termino la linea */
		fx(1,p2," ");
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	agregar archivos utiles si no los pidieron
 *
 * -----------------------------------------------------------------------------------
 */


int	agregar_fflog(s)
char	*s;
{
	fflog = 1;
	strcpy(flog,s);

	if ( fflog && ((hflog = fopen (flog,"w")) == NULL) )
	{
		error(124);
	}
}

int	agregar_ffaux(s)
char	*s;
{
	ffaux = 1;
	strcpy(faux,s);

	if ( ffaux && ((hfaux = fopen (faux,"w")) == NULL) )
	{
		error(121);
	}
}

int	agregar_ffsta(s)
char	*s;
{
	ffsta = 1;
	strcpy(fsta,s);

	if ( ffsta && ((hfsta = fopen (fsta,"w")) == NULL) )
	{
		error(122);
	}
}

int	agregar_ffckf(s)
char	*s;
{
	ffckf = 1;
	strcpy(fckf,s);

	if ( ffckf && ((hfckf = fopen (fckf,"w")) == NULL) )
	{
		error(123);
	}
}

int	agregar_ffdbg(s)
char	*s;
{
	ffdbg = 1;
	strcpy(fdbg,s);

	if ( ffdbg && ((hfdbg = fopen (fdbg,"r")) == NULL) )
	{
		error(124);
	}
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	devuelve nombre de file base + fecha, hora 
 *
 * -----------------------------------------------------------------------------------
 */


char	*fname(bname) 
char	*bname;
{
    static char 	nombre_completo[MAXP];
    char 		fecha_hora[MAXF];

    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    /* Formatear la fecha y hora */
    snprintf(fecha_hora, sizeof(fecha_hora), "_%02d%02d%02d_%02d%02d",
		tm.tm_year % 100, 
		tm.tm_mon + 1, 
		tm.tm_mday,
		tm.tm_hour,
		tm.tm_min);

    /* Crear el nombre completo */
    snprintf(nombre_completo, sizeof(nombre_completo), "%s%s", bname, fecha_hora);

    return nombre_completo;
}






/*
 * -----------------------------------------------------------------------------------
 *
 *	devuelve variables fortran en un string
 * -----------------------------------------------------------------------------------
 */



#if 1


int rutina_v(string,nro,vector_ptr) 
char	*string;
int	*nro;
char	**vector_ptr;
{
    int count = 0;
    char *token;
    char *separators = " _"; // Separadores permitidos (blancos y '_')
    char *temp_string = strdup(string); // Hacemos una copia temporal del string
    char *str_ptr = temp_string;

    // Tokenizamos el string de acuerdo con los separadores
    while ((token = strtok(str_ptr, separators)) != NULL) 
    {
	str_ptr = NULL; // Para continuar tokenizando el string original
	
	// Validamos la sintaxis de str1 (debe comenzar con una letra y puede seguir con letras o números)
	if (isalpha(token[0])) 
	{
	    int valid = 1;
	    for (int i = 1; i < strlen(token); i++) 
	    {
		if (!isalnum(token[i])) 
		{
		    valid = 0; // No válido si tiene algo que no sea letra o número
		    break;
		}
	    }

	    if (valid) 
	    {
		vector_ptr[count] = token; // Guardamos el puntero al token válido
		count++;
	    }

	}
    }

    // Liberamos la memoria temporal
    free(temp_string);
    
    *nro = count; // Guardamos el número de variables encontradas

    return count;     // Retornamos cant de variables encontradas
}



#if 0
int main() {
    char string[1024] = "str1 str2 str2_str2";
    int nro = 0;
    char *vector[100]; // Vector para almacenar punteros a las variables encontradas

    // Llamada a la rutina
    rutina_v(string, &nro, vector);

    // Imprimimos los resultados
    printf("Variables encontradas: %d\n", nro);
    for (int i = 0; i < nro; i++) {
	printf("Variable %d: %s\n", i + 1, vector[i]);
    }

    return 0;
}

#endif


#endif












/*
 * -----------------------------------------------------------------------------------
 *
 *	(MMM)
 *
 *	Rutinas generales gp_xxxxxx
 *
 *
 * -----------------------------------------------------------------------------------
 */


/* general gp algo */
#if 1


/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_print
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_print()
{
	int i;

	printf ("\n");
	printf ("Uso cfg arc       : %2d\n",ffcfg);
	printf ("Cantidad de par   : %2d\n",gp_fq(GP_GET,0) - 1 );
	for (i=0; i<4; i++)
		printf ("Cantidad de par %d : %2d\n",i+1,gp_tpar[i]);
	printf ("\n");
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_parser
 *
 *	parser general de parametros de input al programa
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_parser()
{
	char	s_opcion[MAXR];	/* opcion menu ... en str */
	int	f1,f2,fg;
	char	b1[MAXB];
	int	k_cnt;
 
	int 	i,j,fl;
	char	prm[MAXV];
	char	exe[MAXV];

	char	var1[MAXB];	/* provisorio !! */
	FILE	*hwi;

	strcpy(s_opcion,"0");

	/* excepcion con verbose, por si lo pusieron al final de la linea, por defecto d5 !!! */

	for (i=0; i < gp_fq(GP_GET,0); i++  )
	{	
		if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'v' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{	gp_verbose = 1;
			strcpy(gp_opciones,"d5");

			if (gp_fverbose("d3"))
				printf ("Parametro encontrado: -v \n");
		}

		if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opciones",6) )
		{	strcpy(gp_opciones,desde_igual( gp_fp(GP_GET,i,(char **)0)));

			if (gp_fverbose("d3"))
				printf ("Parametro encontrado: -opciones \n");
		}

		if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'x' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{	
			strcpy (exe, ( gp_fp(GP_GET,i,(char **)0) + 2 ));
			strcpy(s_opcion,exe);

			/* pongo defaults si no quiere usar -cfg=archio ... */
			ffcfg=1;
			strcpy(var1,"tfor.cfg");

			if (gp_fverbose("d3"))
				printf ("Parametro encontrado: -x |%s| \n",s_opcion);
		}
	}


#if 1

	/* Experimentos .... 
	 * Archivo de configuracion (tipo menu)
	 */

	/* si selecciono archivo de configuracion */
	for (i=0, fl=1; fl && i < gp_fq(GP_GET,0); i++)
	{
		if (gp_fverbose("d3"))
			printf ("Arg: %2d  s:|%s| \n", i, gp_fp(GP_GET,i,(char **) 0 ));

		/* parameter type 3 ... "-someoption=somename" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
				*( gp_fp(GP_GET,i,(char **)0) + 1) != '-' && tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"cfg",3) )
			{
				strcpy(var1,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffcfg=1;
			}
		}


		if (ffcfg)
		{
			if ( ffcfg && ((hwi = fopen (var1,"r")) == NULL) )
				error(999);

			fnq1 = &fnp[0];
			qfv_load(hwi,fnq1,&qf_lin);
			
			if (gp_fverbose("d3"))
			{	printf ("Cant lineas arc de configuracion: %d\n",qf_lin);
				for (j=0; j< qf_lin; j++)
					printf ("Par %2d |%s| \n",j,(*fnp[j]).l);
			}

			fclose(hwi);

			k_cnt = 0;

			for (j=0, f2=1, fg=0; f2 && j<qf_lin; j++)
			{

				f1 = 1;
				if ( (*fnp[j]).l[0] == '#')
					f1 = 0;
				if ( (*fnp[j]).l[0] == ' ')
					f1 = 0;
				if ( (*fnp[j]).l[0] ==  0 )
					f1 = 0;


				/* revisar !!!! aca deberiamos elegir la opcion a ejecutar  */
				if (f1)
				{		

					if ( !strncmp( pasar_a_minusc( (*fnp[j]).l ),"opcion",6) ) 
					{
						strcpy(b1, desde_igual( (*fnp[j]).l ));


						if ( !strcmp(b1,s_opcion))
						{	fg = 1;
						}
					}

					if ( strncmp( (*fnp[j]).l,"opcion",6) && strncmp( (*fnp[j]).l,"end",3 ) )
					{
						if (fg)
						{	
							mis_argv = &mis_args[0];
							mis_args[ mis_argc ] = malloc ( strlen( (*fnp[j]).l ) + 2 );
							strcpy ( mis_args[mis_argc] , ( *fnp[j]).l );

							mis_argv++;
							mis_argc++;
						}

					}


					if ( !strncmp( (*fnp[j]).l,"end",3) ) 
					{
						if ( !strcmp(b1,s_opcion))
						{
							fl = 0;
							f2 = 0;
						}
					}
				} /* if (f1) */
			} /* for (j=0 ... */
		} /* if (ffcfg ... */
	} /* for (i=0 ... analisis si selecciono archivo de conf */


	mis_argv = &mis_args[0];
	if (gp_fverbose("d3"))
	{
		printf ("Arch de config solicit : %2d (ffcfg)  \n",ffcfg);
		printf ("Cantidad de argumentos : %2d          \n",mis_argc);
		printf ("\n");

		for (j=0; j<mis_argc; j++)
			printf ("Arg nro: %2d  str: |%-20.20s|  ptr: |%-20-20s|  \n",j,mis_args[j], *mis_argv++);
	}


	mis_argv = &mis_args[0];
	/* si pidio usar archivo de configuracion, reseteo pointers a args */
	if (ffcfg)
	{
		mis_argv = &mis_args[0];
		gp_init(mis_argc,mis_argv);
	}




	if (gp_fverbose("d3"))
	{
		printf ("\n");
		printf ("Argumentos despues de correspondientes gp_init ... \n");
		printf ("\n");
		printf ("cantidad de args : %d  \n",   gp_fq(GP_GET,0) );
		for (i=0; i < gp_fq(GP_GET,0); i++  )
		{
			printf ("arg(%2d) |%s| \n",i,  gp_fp(GP_GET,i,(char **)0)   );
		}

	}

#endif

	/* Seguimos con el proceso normal de analisis de parametros ingresados 
	 * ffcfg 0 - parametros tomados de linea de comando 
	 * ffcfg 1 - pidio usar archivo de configuracion - parametros tomados del archivo
	 *
	 */
	

	/* procesamos todos los parametros existentes */
	for (i=0; i < gp_fq(GP_GET,0);  )
	{
		fl = 1;

		/* parameter type 1 ... "name" */
		if ( i && fl &&  *( gp_fp(GP_GET,i,(char **)0) + 0) != '-')
		{
			fl = 0;
			gp_q_partype1++;
			gp_tpar[0]++;

			if (gp_q_partype1 == 1)
			{
				strcpy(finp, gp_fp(GP_GET,i,(char **)0));
				ffinp=1;
			}

			if (gp_q_partype1 == 2)
			{
				strcpy(fout, gp_fp(GP_GET,i,(char **)0));
				ffout=1;
			}

			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 1: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}


		/* parameter type 2 ... "-something" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
				*( gp_fp(GP_GET,i,(char **)0) + 1) != '-' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			fl = 0;
			gp_q_partype2++;
			gp_tpar[1]++;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'v'  )
				gp_verbose = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'h'  )
				gp_help = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'm'  )
				gp_minusculas = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'f'  )
				gp_fsentencia = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'e'  )
				gp_eol = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 't'  )
				gp_tabs = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'r'  )
				gp_rellena = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'i'  )
				gp_reidx = 1;

			if ( *( gp_fp(GP_GET,i,(char **)0) + 1) == 'p'  )
				gp_pause = 1;

			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 2: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}


		/* parameter type 3 ... "-someoption=somename" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
				*( gp_fp(GP_GET,i,(char **)0) + 1) != '-' && tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			fl = 0;
			gp_q_partype3++;
			gp_tpar[2]++;

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"inp",3) )
			{
				strcpy(finp,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffinp=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"in2",3) )
			{
				strcpy(fin2,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffin2=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"in3",3) )
			{
				strcpy(fin3,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffin3=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"out",3) )
			{
				strcpy(fout,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffout=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou2",3) )
			{
				strcpy(fou2,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou2=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou3",3) )
			{
				strcpy(fou3,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou3=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou4",3) )
			{
				strcpy(fou4,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou4=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou5",3) )
			{
				strcpy(fou5,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou5=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou6",3) )
			{
				strcpy(fou6,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou6=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ou7",3) )
			{
				strcpy(fou7,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffou7=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"aux",3) )
			{	
				strcpy(faux,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffaux=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"log",3) )
			{	
				strcpy(flog,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				fflog=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"sta",3) )
			{	
				strcpy(fsta,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffsta=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"ckf",3) )
			{	
				strcpy(fckf,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffckf=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"dbg",3) )
			{	
				strcpy(fdbg,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffdbg=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"src",3) )
			{	
				strcpy(fsrc,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffsrc=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"lst",3) )
			{	
				strcpy(flst,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				fflst=1;
			}



			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"nvd",3) )
			{	
				gp_niveldes = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opt",3) )
			{	
				gp_suboption = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"cab",3) )
			{	
				gp_cborrar = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"opciones",6) )
			{	
				strcpy(gp_opciones,desde_igual( gp_fp(GP_GET,i,(char **)0)));
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"debug",5) )
			{	
#if 0
				strcpy(gp_debug_st,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				gp_debug = atoi(gp_debug_st);
#endif
#if 1
				gp_debug = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
#endif
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"zap",3) )
			{	
#if 0
				strcpy(gp_proceed_st,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				gp_proceed = atoi(gp_proceed_st);
#endif
#if 1
				gp_proceed = *desde_igual( gp_fp(GP_GET,i,(char **)0)) - '0';
#endif
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"dato",4) )
			{	
				strcpy(gp_dato,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffdat=1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"prue",4) )
			{	
				strcpy(gp_pruebas,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffprb=gp_pruebas[0]-'0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"exec",4) )
			{	
				strcpy(gp_exec,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffexc=gp_exec[0]-'0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"proc",4) )
			{	
				strcpy(gp_proc,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffpro=gp_proc[0]-'0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"tool",4) )
			{	
				strcpy(gp_tool,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				fftoo=gp_tool[0]-'0';
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+1,"util",4) )
			{	
				strcpy(gp_util,desde_igual( gp_fp(GP_GET,i,(char **)0)));
				ffuti=gp_util[0]-'0';
			}



			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 3: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}



		/* parameter type 4 ... "--something" */
		if ( i && fl && *( gp_fp(GP_GET,i,(char **)0) + 0) == '-' && 
				*( gp_fp(GP_GET,i,(char **)0) + 1) == '-' && !tiene_igual( gp_fp(GP_GET,i,(char **)0) ) )
		{
			fl = 0;
			gp_q_partype4++;
			gp_tpar[3]++;

			if (gp_q_partype4 == 1)
			{
				strcpy(var1, gp_fp(GP_GET,i,(char **)0) + 2);
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"version",7) )
			{	
				gp_vers = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgcom",6) )
			{	
				ffchg_com = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgdec",6) )
			{	
				ffchg_dec = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgtyp",6) )
			{	
				ffchg_typ = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chglco",6) )
			{	
				ffchg_lco = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgmas",6) )
			{	
				ffchg_mas = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgini",6) )
			{	
				ffchg_ini = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgvcb",6) )
			{	
				ffchg_vcb = 1;
			}

			if (!strncmp(gp_fp(GP_GET,i,(char **)0)+2,"chgalo",6) )
			{	
				ffchg_alo = 1;
			}



			if (gp_fverbose("d5"))
			{
				printf ("Param tipo 4: %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}


		/* program name */
		if (i==0)
		{	
			if (gp_fverbose("d5"))
			{
				printf ("Programa    : %s\n", gp_fp(GP_GET,i,(char **)0 ) );
			}
		}

		/* next ... */
		i++;
	}

	/* futuro ...
	 * Hacer una gramatica para la combinacion de comandos
	 * posibles !!
	 *
	 * Por ahora, revisamos combinaciones basicas que
	 * son necesarias entre parametros ...
	 *
	 */

	/* si no pone parametros ... mandar a uso */
	if ( (gp_q_partype1 + gp_q_partype2 + gp_q_partype3 + gp_q_partype4 == 0 ) || fl )
		gp_help = 1;

#if 0
	/* si no indico ningun  archivo ... mandar a uso */
	if ( gp_q_partype1 + gp_q_partype3 == 0 )
		gp_help = 1;
#endif 

	if (gp_fverbose("d5"))
	{
		printf ("\n\n");
	}
} 



/*
 * -----------------------------------------------------------------------------------
 *
 *	tiene_igual
 *
 * -----------------------------------------------------------------------------------
 */


int	tiene_igual(s)
char	*s;
{
	int i,flag;

	for (i=0, flag=0; i< strlen(s) && !flag; i++ )
		if ( *(s+i) == '=')
			flag=1;

	return flag;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	desde_igual
 *
 * -----------------------------------------------------------------------------------
 */


char	*desde_igual(s)
char	*s;
{
	int i,flag;

	for (i=0, flag=0; i< strlen(s) && !flag; i++ )
		if ( *(s+i) == '=')
			flag=1;

	return s+i;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_init
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_init(vpar_q,vpar_p)
int	vpar_q;
char	**vpar_p;
{
	int i;

	gp_fq(GP_SET,vpar_q);
	gp_fp(GP_SET,0,vpar_p);
}


/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_fp
 *
 * -----------------------------------------------------------------------------------
 */

char	*gp_fp(action,offset,value)
int	action;
int	offset;
char	**value;
{
	static char **gp_value;

	if (action == GP_SET)
	{	
		gp_value=value;
	}

	return 	*(gp_value+offset);
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_fq(action,value)
 *	
 *	int action
 *	int value
 *	
 *	value:
 *	on fist call, value is a number (typically argc )
 *	after first call, value can be any number, and is not used
 *
 *	returns:
 *	value
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_fq(action,value)
int	action;
int	value;
{
	static int gp_value=0;

	if (action == GP_SET)
	{	
		gp_value=value;
	}

	return gp_value;
}



/*
 * -----------------------------------------------------------------------------------
 *
 *	gp_test 
 *
 *	
 *
 * -----------------------------------------------------------------------------------
 */


int	gp_test()
{
	int i;

	printf ("Uso cfg arc       : %2d\n",ffcfg);

	printf ("Cantidad de par   : %2d\n",gp_fq(GP_GET,0) - 1 );
	for (i=0; i<4; i++)
		printf ("Cantidad de par %d : %2d\n",i+1,gp_tpar[i]);

	printf ("\n");
	for (i=0; i< gp_fq(GP_GET,0); i++)
		printf ("Parametro  %2d     : %s\n",i,gp_fp(GP_GET,i,(char **)0)  );
			
	printf ("\n");
	printf ("\n");
}

/*
 * -----------------------------------------------------------------------------------
 *
 * 	linea_vacia
 *
 *	Determina si una linea esta vacia (generalmente, para lineas leidas de files)
 *	La linea NO esta vacia si contiene al menos 1 caracter distinto de
 *	' ' 	blanco
 *	'\t' 	tab
 *	'\n'	new line
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	linea_vacia(s)
char	*s;
{
	int i,flag;

	i=0;
	flag=1;

	for (i=0; flag && i< strlen(s); i++)
		if (s[i] != ' ' && s[i] != '\t' && s[i] != '\n' )
			flag=0;

	return flag;
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	error
 *
 *	display de errores
 *	en esta version es display de un numero
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	error(value)
int	value;
{
	printf ("Error: %d\n",value);
	if (!value)
		printf ("usar -h para help\n");
	exit(0);
}





/*
 * -----------------------------------------------------------------------------------
 *
 *	tipo_char
 *
 *	ATENCION:
 *	Falta contemplar que hacemos con el caracter '_' !!!!
 *	Por ahora lo incluyo en letras, porque si no, me separa el tag de las palabras
 *	Si voy a parsear corpus sin tag ... lo tengo que sacar de la lista !!!
 *
 *	0	fin de linea
 *	1	blanco o tab
 *	2	otra cosa ( x ahora ... )
 *
 * -----------------------------------------------------------------------------------
 */



int	tipo_char(c)
char	c;
{
	int x;

	x=TC_RST;

	/* para archivos fortran de hita */
	if (c == '\n' || c == 13 )
		x = TC_EOL;

	if (c == ' ' || c == '\t' )
		x = TC_BLA;

	if (c == '_' )
		x = TC_GBJ;

	if (c == ',' || c == ';' || c == ':' || c == '-' || c == '/' || c == '\\' )
		x = TC_CCE;

	if (c == '.' )
		x = TC_PNT;

	if (c == '(' || c == '[' || c == '{' )
		x = TC_PAA;

	if (c == ')' || c == ']' || c == '}' )
		x = TC_PAC;

	if (c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z' )
		x = TC_LET;

	/* fortran */
	if (c >= 'a' && c <= 'z' || c>= 'A' && c <= 'Z' || c == '_')
		x = TC_LET;

	if (c >= '0' && c <= '9' )
		x = TC_NUM;

	/* agrego los que vienen de analizar fuentes fortran */
	if (c == '\'' || c == '%' || c == '!' || c == '*' )
		x = TC_CVR;

	if (c == '"' || c == '&' || c == '+' || c == '=' || c == '>' || c == '<' || c == '?' )
		x = TC_CVR;

	/* agrego los que vienen de analizar fuentes fortran */
	if (c == '$' || c == '#' || c == '^' || c == '@' )
		x = TC_CVR;

	/* encontrado en UN fuente fortran ....  */
	if (c == '|'  )
		x = TC_CVR;

	/* encontrado en UN fuente fortran ....  */
	if (c == '~'  )
		x = TC_CVR;







	if (c == 0)
		x = TC_CHO;

	return x;
}





/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_numero
 *
 *	funcion para determinar si caracter es numero
 *
 *
 * -----------------------------------------------------------------------------------
 */




int	es_numero(c)
char	c;
{
	int x;

	x=0;
	if ( c >= '0' && c <= '9' )
		x=1;

	return x;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_num_tk
 *
 *	funcion para determinar si string es numero
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	es_num_tk(s)
char	*s;
{
	int i,x;

	x=1;
	for (i=0; x && i<strlen(s); i++)
	{	if (s[i] < '0' || s[i] > '9' )
			x=0;

	}

	return x;
}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_puntuacion
 *
 *	funcion para determinar si es simbolo de puntuacion
 *
 *
 * -----------------------------------------------------------------------------------
 */




int	es_puntuacion(s)
char	*s;
{
	int i,x;

	x=1;

	for (i=0; x && i<strlen(s); i++)
	{	if (s[i] != '.' && s[i] != ',' && s[i] != ':' && s[i] != ';' )
			x=0;
	}

	return x;

}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	es_word
 *
 *	funcion para determinar si es word o no
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	es_word(s)
char	*s;
{
	int i,x;

	x=1;

	for (i=0; x && i<strlen(s); i++)
	{	if (s[i] < 'A' || s[i] > 'z' || ( s[i] > 'Z' && s[i] < 'a')  )
			x=0;
	}

	return x;

}

/*
 * -----------------------------------------------------------------------------------
 *
 * 	char_demed
 *
 *	caracteres utilizados normalmente en frases de medidas 
 *	1 cm x 1 cm ( 'x' ) y similares
 *
 *
 * -----------------------------------------------------------------------------------
 */


int	char_demed(c)
char	c;
{
	int	x;

	x=0;
	if ( c == 'e' || c == 'E' || c == 'x' || c == 'X' )
		x=1;

	return x;
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	uso
 *
 *	texto para describir el uso de la herramienta
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_uso(x)
int	x;
{
	char	w[MAXV];
	char	z[MAXV];
	
	sprintf (z, gp_fp(GP_GET,0,(char **)0)  );
	memset (w,0,MAXV);
	strncpy (w,"                                        ",strlen(z));

	printf ("Usage: \n\n");
	printf ("                                                                                                  \n");
	printf ("%s --version                           numero de version  / compilacion                           \n",z);
	printf ("%s -h                                  help                                                       \n",z);
	printf ("%s -v                                  verbose ... muestra cierta informacion de proceso          \n",z);
	printf ("%s -v -opciones=AxByCz...              info: A,B,C = (D)ebug, (I)nformative, (E) extra x=(0-5)    \n",z);
	printf ("%s                                     no imprime, 1 basico, 2 y 3 debug, 4 full debug        \n",w);
	printf ("%s -cfg=archivo.txt -xN                utiliza un archivo de config para pars ... opcion N        \n",z);
	printf ("                                                                                                  \n");
	printf ("Tools:                                                                                            \n");
	printf ("                                                                                                  \n");
	printf ("tool2:         extrae fuentes utilizados por archivo makefile                                     \n");
	printf ("%s -v -opciones=d5 -tool=2 -inp=makefile -out=l1                                                  \n",z);
	printf ("%s -v -opciones=d5 -tool=2 -inp=makefile -out=l1 -m             (in lower case)                   \n",z);
	printf ("                                                                                                  \n");
	printf ("tool3:         carga file con listado de archivos y genera mismo pasado a minusculas              \n");
	printf ("%s -v -opciones=d5 -tool=3 -inp=file  -out=file_to_min                                            \n",z);
	printf ("                                                                                                  \n");
	printf ("tool5:         carga un fuente - procesos varios - genera nuewvo fuente                           \n");
	printf ("%s -v -opciones=d5 -tool=5 -inp=f_org -out=f_new -aux=parser.err -f                               \n",z);
	printf ("%s -v -opciones=d5 -tool=5 -inp=f_org -out=f_new -aux=parser.err     (version graba tokens )      \n",z);
	printf ("                                                                                                  \n");
	printf ("tool6:         carga un fuente - opciones para arreglos varios - genera nuevo fuente              \n");
	printf ("%s -v -opciones=d5 -tool=6 -inp=f_org -out=f_new -aux=parser.err                                  \n",z);
	printf ("      --chgcom  convierte lineas comentadas                                                       \n");
	printf ("      --chgtyp  arregla especificacion de variables (kind,len) pone los :: en int log real char   \n");
	printf ("      --chglcp  arregla lineas de continuacion ... reemplaza + por &                              \n");
	printf ("                                                                                                  \n");
	printf ("prue2:         carga todos los archivos en list_src a  memo y los genera en new_repo              \n");
	printf ("%s -v -opciones=d5 -prue=2 -inp=list_src -out=l_names -aux=p.err -dato=new_repo                   \n",z);
	printf ("                                                                                                  \n");
	printf ("                                                                                                  \n");



	if (gp_fverbose("d1"))
		printf ("gp_uso(%d)\n",x);

	printf ("\n\n\n");

	exit(x);
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	gp_default
 *
 * 	inicializa parametros de funcionamiento default
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_default()
{
	gp_help=0;
	gp_vers=0;
	gp_verbose=0;

	ffcfg=0;
	ffinp=0;
	ffin2=0;
	ffout=0;
	ffou2=0;
	ffou3=0;
	ffou4=0;
	ffou5=0;
	ffou6=0;
	ffou7=0;
	ffaux=0;
	fflog=0;
	ffsta=0;
	ffckf=0;

	ffsrc=0;
	fflst=0;

	ffdat=0;

	sprintf (gp_opciones, "%s","______");
	sprintf (gp_dato    , "%s","______");
	sprintf (gp_pruebas , "%s","______");
	sprintf (gp_exec    , "%s","______");
	sprintf (gp_proc    , "%s","______");
	sprintf (gp_tool    , "%s","______");
	sprintf (gp_util    , "%s","______");
	sprintf (gp_reportes, "%s","saludo");


	memset(gp_tpar,0,sizeof(gp_tpar));

}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	version
 *
 *	version del codigo
 *
 * -----------------------------------------------------------------------------------
 */

int	gp_version(x)
int	x;
{
	static	char	ver[MAXB];
	char	d[MAXV];
	char	w[MAXV];
	char	z[MAXB];

	strcpy (ver,"0061");
	strcpy (d," Thu Feb 27 07:04:09 -03 2025");

	sprintf (z,"%s -- (%s)  %s", gp_fp(GP_GET,0,(char **)0), ver, d  );
	memset (w,0,MAXV);
	strncpy (w,"                                        ",strlen(z));

	if (x)
	{
		printf ("%s\n",z);
		printf ("\n\n");
	}

	if (x)
		exit(x);
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	minusculas (version chatgpt )
 *
 * -----------------------------------------------------------------------------------
 */

// Función auxiliar para convertir una cadena a minúsculas

char *minusculas(char *s) 
{
    char *ptr = s;
    while (*ptr) 
    {
	*ptr = tolower(*ptr);
	ptr++;
    }
    return s;
}

/*
 * -----------------------------------------------------------------------------------
 *
 * 	pasar_a_minusc
 *
 * -----------------------------------------------------------------------------------
 */

char	*pasar_a_minusc(s)
char	*s;
{
	static char b[MSTR];

	int i,j,k;

	strcpy(b,s);

	for (i=0; i<MAXB && b[i]; i++)
	{
		if ( b[i] >= 'A' && b[i] <= 'Z' )
			b[i] += 32;
	}

	return b;
}


/*
 * -----------------------------------------------------------------------------------
 *
 * 	gp_fverbose
 *
 * -----------------------------------------------------------------------------------
 */


/*
 *	gp_fverbose
 *
 *
 *	Devuelve 1 (si) o 0 (no) si aplica la condicion de verbose
 *	
 *	Las opciones involucradas son:
 *	-v               activa el verbose
 *	-opciones=str    string de condiciones de verbose  (requiere -v )
 *	                 
 *	El string de opciones es del tipo "Ln  (L)=I,D,E  (n)=0,1,2,3,4,5   "
 *	Donde:
 *	L       Es la letra (I informativo, D debug, y E ... no me acuerdo
 *	n       Es el nivel
 *	        0 no imprime nada   ( es lo mismo que no poner nada !!! )
 *	        1    imprime cosas basicos, por que rutinas paso etc
 *	        2,3  niveles de debug intermedios
 *	        4    full debug
 *	        5    incluye debug de gp_parse
 *
 * 
 *
 *	
 */


int	gp_fverbose(situacion)
char	*situacion;
{
	int	i,j,k;
	int	sit;

	static	int	f_p=1;
	static	int	f_i=0;
	static	int	f_e=0;
	static	int	f_d=0;

	if ( f_p == 1 && gp_opciones[0] != '_' )
	{	f_p = 0;

		for (i=0; i<strlen(gp_opciones)-1; i=i+2)
		{
			if (*(gp_opciones+i) == 'i' || *(gp_opciones+i) == 'I' )
				f_i = (int) *(gp_opciones+i+1) - '0';
			if (*(gp_opciones+i) == 'e' || *(gp_opciones+i) == 'E' )
				f_e = (int) *(gp_opciones+i+1) - '0';
			if (*(gp_opciones+i) == 'd' || *(gp_opciones+i) == 'D' )
				f_d = (int) *(gp_opciones+i+1) - '0';
		}
#if 0
	printf ("gp_fverbose:  f_i : %d\n",f_i);
	printf ("gp_fverbose:  f_e : %d\n",f_e);
	printf ("gp_fverbose:  f_d : %d\n",f_d);
#endif
	}


	sit = 0;

	if (gp_verbose)
	{

		/* determino si hay que devolver 0 o 1
		 * en base a situacion informada, y 
		 * conjunto de flags y '-opcion=vN'   
		 */
	
		for (sit=0, i=0; i<strlen(situacion); i=i+2)
		{
#if 0
			printf ("X1 f_P: %d i:%d situaci: %s  opcion: %s f_d: %d sit: %d\n",
				f_p,i,situacion,gp_opciones,f_d,sit);
#endif
	
			if (*(situacion+i) == 'i')
				if ( *(situacion+i+1)-'0' <= f_i )
					sit = 1;
	
			if (*(situacion+i) == 'e')
				if ( *(situacion+i+1)-'0' <= f_e )
					sit = 1;
	
			if (*(situacion+i) == 'd')
				if ( *(situacion+i+1)-'0' <= f_d )
					sit = 1;
#if 0
			printf ("X2 f_P: %d i:%d situaci: %s  opcion: %s f_d: %d sit: %d\n",
				f_p,i,situacion,gp_opciones,f_d,sit);
#endif
		}
	}

	return	sit;
}




/*
 * -----------------------------------------------------------------------------------
 *
 * 	gp_tm
 *
 * -----------------------------------------------------------------------------------
 */




char	*gp_tm()
{
	time_t t;
	struct tm *tm;

	t=time(NULL);
	tm=localtime(&t);
	strftime(gp_fyh, MAXR , "[%Y/%m/%d %H:%M:%S]", tm);

	return(gp_fyh);
}



/*
 * -----------------------------------------------------------------------------------
 *
 * 	df
 *
 * -----------------------------------------------------------------------------------
 */


char	*df(s)
char	*s;
{
	static	char	nf[MAXF];

	int	i,j,k,f1;

	for ( f1=1, i=0; f1 && i<strlen(s); i++)
		if (s[i] != '.')
			nf[i]=s[i];
		else
			nf[i]=0,f1=0;

	strcat(nf,".dif");


	return nf;
}


#endif
/* general gp algo */







/*
 * -----------------------------------------------------------------------------------
 *	fin de codigo util - comienza exploracion
 *	fin de codigo util - comienza exploracion
 *	fin de codigo util - comienza exploracion
 * -----------------------------------------------------------------------------------
 */




#if 0

#include<stdio.h>
#include <string.h>

int main() {
   char string[50] = "Hello! We are learning about strtok";
   // Extract the first token
   char * token = strtok(string, " ");
   // loop through the string to extract all other tokens
   while( token != NULL ) {
      printf( " %s\n", token ); //printing each token
      token = strtok(NULL, " ");
   }
   return 0;
}


#endif



#if 0



#include <stdio.h>
#include <string.h>
#include <time.h>

char* archivo_log(char *nombre_log) 
{
    static char nombre_completo[256];
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    // Formatear la fecha y hora
    char fecha_hora[20];
    snprintf(fecha_hora, sizeof(fecha_hora), "_%02d%02d%02d_%02d%02d",
	     tm.tm_year % 100, tm.tm_mon + 1, tm.tm_mday,
	     tm.tm_hour, tm.tm_min);

    // Crear el nombre completo
    snprintf(nombre_completo, sizeof(nombre_completo), "%s%s", nombre_log, fecha_hora);

    return nombre_completo;
}

int main()
{
    char nombre_log[256];
    strcpy(nombre_log, "prueba");

    strcpy(nombre_log, archivo_log(nombre_log));
    printf("Nombre del archivo: %s\n", nombre_log);

    return 0;
}




#endif




#if 0



#include <stdio.h>
#include <string.h>
#include <stdbool.h>

bool tiene_include(char *s, char *t) 
{
    // Define la cadena a buscar
    const char *pattern = "   include  '";
    int pattern_len = strlen(pattern);

    // Busca el patrón en el string s
    char *start = strstr(s, pattern);

    if (start != NULL) 
    {
	// Encuentra el inicio del nombre de archivo
	start += pattern_len;
	char *end = strchr(start, '\'');
	
	if (end != NULL) 
	{
	    // Copia el nombre de archivo sin las comillas en t
	    strncpy(t, start, end - start);
	    t[end - start] = '\0';
	    return true;
	}
    }

    // No se encontró el patrón
    return false;
}

int main() 
{
    char s[] = "Este es un ejemplo de cadena con   include  'nombre_de_file_valido' en el medio.";
    char t[100];

    if (tiene_include(s, t)) 
    {
	printf("Se encontró la sentencia. El nombre de archivo es: %s\n", t);
    }
    else 
    {
	printf("No se encontró la sentencia.\n");
    }

    return 0;
}




#endif



#if 0



#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

bool tiene_include(char *s, char *t) {
    // Define la cadena a buscar
    const char *keyword = "include";
    char *start = strstr(s, keyword);

    if (start != NULL) {
	// Avanza el puntero más allá de "include"
	start += strlen(keyword);

	// Salta los espacios en blanco
	while (isspace(*start)) {
	    start++;
	}

	// Verifica si el siguiente carácter es un apóstrofe
	if (*start == '\'') {
	    start++; // Salta el apóstrofe inicial

	    // Encuentra el apóstrofe final
	    char *end = strchr(start, '\'');
	    if (end != NULL) {
		// Copia el nombre de archivo sin las comillas en t
		strncpy(t, start, end - start);
		t[end - start] = '\0';
		return true;
	    }
	}
    }

    // No se encontró el patrón
    return false;
}

int main() {
    char s[] = "Este es un ejemplo de cadena con include    'nombre_de_file_valido' en el medio.";
    char t[100];

    if (tiene_include(s, t)) {
	printf("Se encontró la sentencia. El nombre de archivo es: %s\n", t);
    } else {
	printf("No se encontró la sentencia.\n");
    }

    return 0;
}




#endif




#if 0

#include <stdlib.h>
#include <string.h>

// Ejemplo de estructura para info
typedef struct {
    char data[MAXB];
    // Otros campos...
} info_t;

// Función de comparación corregida
int info_cmp1(const void *a, const void *b) {
    // Convertir punteros void a punteros info_t
    const info_t *ia = (const info_t *)a;
    const info_t *ib = (const info_t *)b;

    // Comparar los campos de la estructura (ajusta según lo necesario)
    return strcmp(ia->data, ib->data);
}

// Otra función de comparación de ejemplo
int info_cmp2(const void *a, const void *b) {
    const info_t *ia = (const info_t *)a;
    const info_t *ib = (const info_t *)b;

    // Comparar los campos de la estructura (ajusta según lo necesario)
    return strcmp(ia->data, ib->data);
}

// Función que usa qsort
void ex3_p2() {
    info_t info[MAXN]; // Ajusta según tu implementación
    int n_info = /* número de elementos en info */;
    
    qsort(&info[1], n_info, sizeof(info_t), info_cmp1);
}

void ex3_p3() {
    info_t info[MAXN]; // Ajusta según tu implementación
    int n_info = /* número de elementos en info */;
    
    qsort(&info[0], n_info, sizeof(info_t), info_cmp2);
}





#endif





#if 0


#include <stdlib.h>
#include <string.h>

#define MAXB  1024

char info[1000][MAXB];
int n_info = 1000;

// Función de comparación corregida
int info_cmp1(const void *p1, const void *p2) 
{
    // Convertir punteros void a punteros char
    const char *a = (const char *)p1;
    const char *b = (const char *)p2;

    return strcmp(a, b);
}

// Ejemplo de uso de qsort
void ex3_p2() 
{
    qsort(&info[0], n_info, MAXB, info_cmp1);
}

int main() 
{
    // Llamada a la función para ordenar
    ex3_p2();

    return 0;
}


#endif










/* end of file  */
/* end of file  */
/* end of file  */
/* end of file  */
