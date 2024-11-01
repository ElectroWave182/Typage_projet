(* Datatypes for Postscript instructions *)


open Lang
open Gen

type instr =
  IVal of value
| IVar of int
| IOper of string
| IBloc of instr
| ISeq of instr list
| ILoop of instr
| IDef of fname * instr



let string_of_val =
function

	| (BoolV b) -> string_of_bool b
	| (FloatV f) -> string_of_float f
	| (IntV i) -> string_of_int i
	| s -> s

;;


let rec string_of_instr =
function
	
	(* Constante *)
	| (IVal valeur) ->
		string_of_val valeur
	
	(* Variable *)
	| (IVar _) ->
		"ci-gît une variable"
	
	(* Opérateur *)
	| (IOper operateur) ->
		oparateur
	
	(* Bloc 'then' ou 'else'*)
	| (IBloc bloc) ->
		"{"
		^ string_of_instr bloc
		^ "} "
	
	(* Séquence *)
	| (ISeq seq) ->
		string_of_instr_list seq
	
	(* Boucle *)
	| (ILoop (ISeq boucle)) ->
		string_of_boucle boucle
	
	(* Définition de fonction *)
	| (IDef (nom, code)) ->
		"/"
		^ nom
		^ " {"
		^ string_of_instr code
		^ "} def\n"


and string_of_instr_list =
function

	(* Cas récursif : la tete de liste est un bloc 'then' *)
	| ((IBloc tete) :: cou :: poitrine :: queue) ->
		string_of_instr (IBloc tete)
		^ string_of_instr cou
		^ "ifelse\n"
		^ string_of_instr_list (poitrine :: queue)
	
	(* Autres cas récursifs *)
	| (tete :: cou :: queue) ->
		string_of_instr tete
		^ " "
		^ string_of_instr_list (cou :: queue)
	
	(* Cas de base : la liste n'a qu'un seul élément *)
	| [dernier] ->
		string_of_instr dernier
	
	(* Cas élémentaire : la liste est vide *)
	| _ -> ""


and string_of_boucle =
function

	(* Cas récursif *)
	| (tete :: cou :: queue) ->
		string_of_instr tete
		^ " "
		^ string_of_boucle (cou :: queue)
	
	(* Cas de base : il ne reste plus qu'une instruction *)
	| [derniere] ->
		"{"
		^ string_of_instr derniere
		^ "} for\n"
	
	(* Cas élémentaire : il n'y a pas d'instruction *)
	| _ -> "{} for\n"

;;



(* - Tests unitaires - *)


string_of_instr instr1 ;;
string_of_instr instr2 ;;
string_of_instr instr3 ;;
string_of_instr instr4 ;;
