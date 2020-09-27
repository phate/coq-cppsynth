Require Import
  Coq.Arith.Compare_dec
  Coq.Numbers.DecimalString
  Coq.Numbers.DecimalNat
  Coq.Strings.String
  Coq.Lists.List
  CPPSynth.CoqAstBase
  CPPSynth.SExpression
  CPPSynth.StringUtil
  CPPSynth.Exception
  CPPSynth.Monad.

Inductive expr_t : Set :=
  | expr_global :
    forall (name : name.t), expr_t
  | expr_local :
    forall (name : name.t) (index : nat), expr_t
  | expr_prod :
    forall (arg : arg_t) (expr : expr_t), expr_t
  | expr_lambda :
    forall (arg : arg_t) (body : expr_t), expr_t
  | expr_letin :
    forall (name : option name.t) (term : expr_t) (term_type : expr_t) (body : expr_t), expr_t
  | expr_case :
    forall (num_args : nat) (case_term : expr_t) (match_term : expr_t) (branches : exprlist_t), expr_t
  | expr_app :
    forall (fn : expr_t) (arg : expr_t), expr_t
  | expr_fixpoint :
    forall (index : nat) (fns : fixlist_t), expr_t

with exprlist_t : Set :=
  | exprlist_nil : exprlist_t
  | exprlist_cons : forall (e : expr_t) (l : exprlist_t), exprlist_t

with fixlist_t : Set :=
  | fixlist_nil : fixlist_t
  | fixlist_cons : forall (f : fixfn_t) (l : fixlist_t), fixlist_t

with fixfn_t : Set :=
  | fixfn : forall (name : name.t) (args : args_t) (type : expr_t) (body : expr_t), fixfn_t

with args_t : Set :=
  | args_nil : args_t
  | args_cons : forall (a : arg_t) (l : args_t), args_t

with arg_t : Set :=
  | arg_named : forall (name : name.t) (type : expr_t), arg_t
  | arg_anonymous : forall (type : expr_t), arg_t
.

Definition arg_make (name : option string) (type : expr_t) : arg_t :=
  match name with
    | Some name => arg_named name type
    | None => arg_anonymous type
  end.

Definition sexpr_parse_name (e : sexpr.t) : ExceptionOr (option string) :=
  match e with
    | sexpr.terminal t => Exception "ParseError" "Expected name expresion"
    | sexpr.expr kind args =>
      if string_dec kind "Anonymous" then
        return_ _ None
      else if string_dec kind "Name" then
        do arg <-- sexpr.split1 args ;
        do name <-- sexpr.as_terminal arg ;
        return_ _ (Some name)
      else
        Exception "ParseError" "Expected 'Name' or 'Anonymous'"
  end.

Fixpoint from_sexpr (e : sexpr.t) : ExceptionOr expr_t :=
  match e with
    | sexpr.terminal t => Exception "ParseError" "Compound expression expected"
    | sexpr.expr kind args =>
      if string_dec kind "Sort" then
        do arg <-- sexpr.split1 args ;
        do name <-- sexpr.as_terminal arg ;
        return_ _ (expr_global name)
      else if string_dec kind "Global" then
        do arg <-- sexpr.split1 args ;
        do name <-- sexpr.as_terminal arg ;
        return_ _ (expr_global name)
      else if string_dec kind "Local" then
        do args <-- sexpr.split2 args ;
        do name <-- sexpr.as_terminal (fst args) ;
        do index <-- sexpr.as_terminal (snd args) ;
        match nat_of_string index with
          | Some index => return_ _ (expr_local name index)
          | None => Exception "ParseError" "Expected integer index"
        end
      else if string_dec kind "Prod" then
        match args with
          | sexpr.list_cons argname (sexpr.list_cons argtype (sexpr.list_cons body sexpr.list_nil)) =>
            do argname' <-- sexpr_parse_name argname ;
            do argtype' <-- from_sexpr argtype ;
            do body' <-- from_sexpr body ;
            return_ _ (expr_prod (arg_make argname' argtype') body')
          | _ => Exception "ParseError" "Product requires 3 arguments"
        end
      else if string_dec kind "Lambda" then
        match args with
          | sexpr.list_cons argname (sexpr.list_cons argtype (sexpr.list_cons body sexpr.list_nil)) =>
            do argname' <-- sexpr_parse_name argname ;
            do argtype' <-- from_sexpr argtype ;
            do body' <-- from_sexpr body ;
            return_ _ (expr_lambda (arg_make argname' argtype') body')
          | _ => Exception "ParseError" "Lambda requires 3 arguments"
        end
      else if string_dec kind "LetIn" then
        match args with
          | sexpr.list_cons name (sexpr.list_cons term (sexpr.list_cons termtype (sexpr.list_cons body sexpr.list_nil))) =>
            do name' <-- sexpr_parse_name name ;
            do term' <-- from_sexpr term ;
            do termtype' <-- from_sexpr termtype ;
            do body' <-- from_sexpr body ;
            return_ _ (expr_letin name' term' termtype' body')
          | _ => Exception "ParseError" "LetIn requires 4 arguments"
        end
      else if string_dec kind "App" then
        match args with
          | sexpr.list_cons fn (sexpr.list_cons arg sexpr.list_nil) =>
            do fn' <-- from_sexpr fn ;
            do arg' <-- from_sexpr arg ;
            return_ _ (expr_app fn' arg')
          | _ => Exception "ParseError" "App requires 2 arguments"
        end
      else
        Exception "ParseError" ("Unknown kind " ++ kind)
  end.

Module inductive_constructor.
  Inductive t :=
    | make :
      forall (name : string) (type : expr_t), t.

  Definition from_sexpr (e : sexpr.t) : ExceptionOr t :=
    match e with
      | sexpr.terminal t => Exception "ParseError" "Compound expression expected"
      | sexpr.expr kind args =>
        if string_dec kind "Constructor" then
          match args with
            | sexpr.list_cons (sexpr.terminal name) (sexpr.list_cons type sexpr.list_nil) =>
              do type' <-- from_sexpr type ;
              return_ _ (make name type')
            | _ => Exception "ParseError" "Product requires 3 arguments"
          end
        else
          Exception "ParseError" ("Expected constructor, received " ++ kind)
    end.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr (list t) :=
    let result := nil in
    (fix loop (l : sexpr.list_t) (result : list t) : ExceptionOr (list t) :=
      match l with
        | sexpr.list_cons e l =>
          do c <-- from_sexpr e ;
          let result := result ++ c :: nil in
          loop l result
        | sexpr.list_nil => Okay result
      end) l result.

  Definition type (this : t) : expr_t := let (name, type) := this in type.
  Definition name (this : t) : string := let (name, type) := this in name.

End inductive_constructor.

Module one_inductive.
  Inductive t : Set :=
    | make :
      forall (name : string),
      forall (type : expr_t),
      forall (cons : list inductive_constructor.t),
      t.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr t :=
    match l with
      | sexpr.list_cons (sexpr.terminal name) (sexpr.list_cons type constrs) =>
        let name' := name in
        do type' <-- from_sexpr type ;
        do constrs' <-- inductive_constructor.from_sexpr_list constrs ;
        return_ _ (make name type' constrs')
      | _ => Exception "ParseError" "OneInductive needs name, type and constructors"
    end.

  Definition from_sexpr (e : sexpr.t) : ExceptionOr t :=
    match e with
      | sexpr.expr kind l =>
        if string_dec kind "OneInductive" then
          from_sexpr_list l
        else
          Exception "ParseError" ("Expected OneInductive, got " ++ kind)
      | _ => Exception "ParseError" "OneInductive needs name, type and constructors"
    end.

End one_inductive.

Module inductive.
  Inductive t : Set :=
    | make :
      forall (parts : list one_inductive.t),
      t.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr t :=
    let result := nil in
    do result <--
      (fix loop (l : sexpr.list_t) (result : list one_inductive.t) : ExceptionOr (list one_inductive.t) :=
        match l with
          | sexpr.list_cons e l =>
            do oi <-- one_inductive.from_sexpr e ;
            let result := result ++ oi :: nil in
            loop l result
          | sexpr.list_nil => Okay result
        end) l result ;
    return_ _ (make result).

End inductive.

Module definition.
  Inductive t : Set :=
    | make :
      forall (name : string),
      forall (type : expr_t),
      forall (body : expr_t),
      t.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr t :=
    do parts <-- sexpr.split3 l ;
    let s_name := (fst (fst parts)) in
    let s_type := (snd (fst parts)) in
    let s_body := snd parts in
    do name <-- sexpr.as_terminal s_name ;
    do type <-- from_sexpr s_type ;
    do body <-- from_sexpr s_body ;
    return_ _ (make name type body).

End definition.

Module structure_field.
  Inductive t : Set :=
    | f_ind : forall (i : inductive.t), t
    | f_def : forall (d : definition.t), t
   .

  Definition from_sexpr (e : sexpr.t) : ExceptionOr t :=
    match e with
      | sexpr.terminal t => Exception "ParseError" "Expected definition or inductive"
      | sexpr.expr kind args =>
        if string_dec kind "Inductive" then
          do i <-- inductive.from_sexpr_list args ;
          return_ _ (f_ind i)
        else if string_dec kind "Record" then
          do i <-- inductive.from_sexpr_list args ;
          return_ _ (f_ind i)
        else if string_dec kind "Definition" then
          do d <-- definition.from_sexpr_list args ;
          return_ _ (f_def d)
        else
          Exception "ParseError" "Expected definition or inductive"
    end.

End structure_field.

Module module.
  (* XXX: nested modules *)
  Inductive t : Set :=
    | make :
      forall (name : string),
      forall (body : list structure_field.t),
      t.

  Definition from_sexpr_list (l : sexpr.list_t) : ExceptionOr t :=
    do parts <-- sexpr.split2 l ;
    let s_name := fst parts in
    let s_body : sexpr.t := snd parts in
    do name <-- sexpr.as_terminal s_name ;
    do s_body_parts <-- sexpr.as_expr s_body ;
    let body_kind := fst s_body_parts in
    if string_dec body_kind "StructureBody" then
      let s_body_args := snd s_body_parts in
      do body <-- sexpr.split1 s_body_args ;
      let result := nil in
      let maybe_result :=
        (fix loop (l : sexpr.list_t) (result : list structure_field.t) : ExceptionOr (list structure_field.t) :=
          match l with
            | sexpr.list_cons e l =>
              do f <-- structure_field.from_sexpr e ;
              let result := result ++ f :: nil in
              loop l result
            | sexpr.list_nil => Okay result
          end) s_body_args result in
      do result <-- maybe_result ;
      return_ _ (make name result)
    else
      Exception "ParseError" "Expected StructureBody".

End module.
