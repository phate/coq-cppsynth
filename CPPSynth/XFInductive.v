Require Import
  Coq.Strings.String
  CPPSynth.CPPAst.

(* When an inductive type is mapped to C++, the following aspects need to be mapped:
- representation for the type itself
- how constructors are invoked
- how pattern matching is facilitated

Presently there is the following option for generating a mapping:
- type is represented by a shallow-copy immutable class
- constructors are static functions of the class
- pattern matching is a member function *)

(* Describe how an inductive Coq type is mapped to C++. This is for code
*using* the inductive type, to determine how it is translated (in 
either dat structures or functional code). *)
Module oind_cpp_repr.
  Record t : Set := mk {
    qid : string (* fully qualified one inductive id *) ;
    cpp_name : string ; (* C++ class name *)
    is_templated : bool ; (* whether this has template parameters *)
  }.
End oind_cpp_repr.


(* Implementation class of a single inductive type. *)
Module oind_cpp_impl.
  Record t : Set := make {
    tpl : tplformargs_t ;
    clsname : string ;
    clsdecls : clsdecls_t
  }.
End oind_cpp_impl.

Module ind_cpp_impl.
  Definition t := list oind_cpp_impl.t.
End ind_cpp_impl.
