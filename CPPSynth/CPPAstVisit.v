Require Import
  CPPSynth.CPPAst.

Module visitor.
  Definition control := bool.
  Section Dependent.
    Variable T : Set.
    Record vmt_t : Set := vmt_make {
      decl_pre : decl_t -> T -> control * T ;
      decl_post : decl_t -> T -> decl_t * T ;
      clsdecl_pre : clsdecl_t -> T -> control * T ;
      clsdecl_post : clsdecl_t -> T -> clsdecl_t * T ;
      clsinherit_pre : clsinherit_t -> T -> control * T ;
      clsinherit_post : clsinherit_t -> T -> clsinherit_t * T ;
      funbody_pre : funbody_t -> T -> control * T ;
      funbody_post : funbody_t -> T -> funbody_t * T ;
      cinit_pre : cinit_t -> T -> control * T ;
      cinit_post : cinit_t -> T -> cinit_t * T ;
      idexpr_pre : idexpr_t -> T -> control * T ;
      idexpr_post : idexpr_t -> T -> idexpr_t * T ;
      scope_pre : scope_t -> T -> control * T ;
      scope_post : scope_t -> T -> scope_t * T ;
      templateid_pre : templateid_t -> T -> control * T ;
      templateid_post : templateid_t -> T -> templateid_t * T ;
      tplformarg_pre : tplformarg_t -> T -> control * T ;
      tplformarg_post : tplformarg_t -> T -> tplformarg_t * T ;
      tplarg_pre : tplarg_t -> T -> control * T ;
      tplarg_post : tplarg_t -> T -> tplarg_t * T ;
      typeexpr_pre : typeexpr_t -> T -> control * T ;
      typeexpr_post : typeexpr_t -> T -> typeexpr_t * T ;
      funtypeexpr_pre : funtypeexpr_t -> T -> control * T ;
      funtypeexpr_post : funtypeexpr_t -> T -> funtypeexpr_t * T ;
      funarg_pre : funarg_t -> T -> control * T ;
      funarg_post : funarg_t -> T -> funarg_t * T ;
      stmt_pre : stmt_t -> T -> control * T ;
      stmt_post : stmt_t -> T -> stmt_t * T ;
      condition_pre : condition_t -> T -> control * T ;
      condition_post : condition_t -> T -> condition_t * T ;
      expr_pre : expr_t -> T -> control * T ;
      expr_post : expr_t -> T -> expr_t * T ;
      binder_pre : binder_t -> T -> control * T ;
      binder_post : binder_t -> T -> binder_t * T
    }.
    Definition default_decl_pre (decl : decl_t) (state : T) : control * T := (true, state).
    Definition default_decl_post (decl : decl_t) (state : T) : decl_t * T := (decl, state).
    Definition default_clsdecl_pre (clsdecl : clsdecl_t) (state : T) : control * T := (true, state).
    Definition default_clsdecl_post (clsdecl : clsdecl_t) (state : T) : clsdecl_t * T := (clsdecl, state).
    Definition default_clsinherit_pre (clsinherit : clsinherit_t) (state : T) : control * T := (true, state).
    Definition default_clsinherit_post (clsinherit : clsinherit_t) (state : T) : clsinherit_t * T := (clsinherit, state).
    Definition default_funbody_pre (funbody : funbody_t) (state : T) : control * T := (true, state).
    Definition default_funbody_post (funbody : funbody_t) (state : T) : funbody_t * T := (funbody, state).
    Definition default_cinit_pre (cinit : cinit_t) (state : T) : control * T := (true, state).
    Definition default_cinit_post (cinit : cinit_t) (state : T) : cinit_t * T := (cinit, state).
    Definition default_idexpr_pre (idexpr : idexpr_t) (state : T) : control * T := (true, state).
    Definition default_idexpr_post (idexpr : idexpr_t) (state : T) : idexpr_t * T := (idexpr, state).
    Definition default_scope_pre (scope : scope_t) (state : T) : control * T := (true, state).
    Definition default_scope_post (scope : scope_t) (state : T) : scope_t * T := (scope, state).
    Definition default_templateid_pre (templateid : templateid_t) (state : T) : control * T := (true, state).
    Definition default_templateid_post (templateid : templateid_t) (state : T) : templateid_t * T := (templateid, state).
    Definition default_tplformarg_pre (tplformarg : tplformarg_t) (state : T) : control * T := (true, state).
    Definition default_tplformarg_post (tplformarg : tplformarg_t) (state : T) : tplformarg_t * T := (tplformarg, state).
    Definition default_tplarg_pre (tplarg : tplarg_t) (state : T) : control * T := (true, state).
    Definition default_tplarg_post (tplarg : tplarg_t) (state : T) : tplarg_t * T := (tplarg, state).
    Definition default_typeexpr_pre (typeexpr : typeexpr_t) (state : T) : control * T := (true, state).
    Definition default_typeexpr_post (typeexpr : typeexpr_t) (state : T) : typeexpr_t * T := (typeexpr, state).
    Definition default_funtypeexpr_pre (funtypeexpr : funtypeexpr_t) (state : T) : control * T := (true, state).
    Definition default_funtypeexpr_post (funtypeexpr : funtypeexpr_t) (state : T) : funtypeexpr_t * T := (funtypeexpr, state).
    Definition default_funarg_pre (funarg : funarg_t) (state : T) : control * T := (true, state).
    Definition default_funarg_post (funarg : funarg_t) (state : T) : funarg_t * T := (funarg, state).
    Definition default_stmt_pre (stmt : stmt_t) (state : T) : control * T := (true, state).
    Definition default_stmt_post (stmt : stmt_t) (state : T) : stmt_t * T := (stmt, state).
    Definition default_condition_pre (condition : condition_t) (state : T) : control * T := (true, state).
    Definition default_condition_post (condition : condition_t) (state : T) : condition_t * T := (condition, state).
    Definition default_expr_pre (expr : expr_t) (state : T) : control * T := (true, state).
    Definition default_expr_post (expr : expr_t) (state : T) : expr_t * T := (expr, state).
    Definition default_binder_pre (binder : binder_t) (state : T) : control * T := (true, state).
    Definition default_binder_post (binder : binder_t) (state : T) : binder_t * T := (binder, state).

    Definition vmt : vmt_t := vmt_make
      default_decl_pre default_decl_post
      default_clsdecl_pre default_clsdecl_post
      default_clsinherit_pre default_clsinherit_post
      default_funbody_pre default_funbody_post
      default_cinit_pre default_cinit_post
      default_idexpr_pre default_idexpr_post
      default_scope_pre default_scope_post
      default_templateid_pre default_templateid_post
      default_tplformarg_pre default_tplformarg_post
      default_tplarg_pre default_tplarg_post
      default_typeexpr_pre default_typeexpr_post
      default_funtypeexpr_pre default_funtypeexpr_post
      default_funarg_pre default_funarg_post
      default_stmt_pre default_stmt_post
      default_condition_pre default_condition_post
      default_expr_pre default_expr_post
      default_binder_pre default_binder_post.

    Definition override_typeexpr_pre fn (vmt : vmt_t) : vmt_t :=
      let (decl_pre, decl_post, clsdecl_pre, clsdecl_post, clsinherit_pre, clsinherit_post, funbody_pre, funbody_post, cinit_pre, cinit_post, idexpr_pre, idexpr_post, scope_pre, scope_post, templateid_pre, templateid_post, tplformarg_pre, tplformarg_post, tplarg_pre, tplarg_post, typeexpr_pre, typeexpr_post, funtypeexpr_pre, funtypeexpr_post, funarg_pre, funarg_post, stmt_pre, stmt_post, condition_pre, condition_post, expr_pre, expr_post, binder_pre, binder_post) := vmt in
      let typeexpr_pre := fn in
      vmt_make decl_pre decl_post clsdecl_pre clsdecl_post clsinherit_pre clsinherit_post funbody_pre funbody_post cinit_pre cinit_post idexpr_pre idexpr_post scope_pre scope_post templateid_pre templateid_post tplformarg_pre tplformarg_post tplarg_pre tplarg_post typeexpr_pre typeexpr_post funtypeexpr_pre funtypeexpr_post funarg_pre funarg_post stmt_pre stmt_post condition_pre condition_post expr_pre expr_post binder_pre binder_post.

    Definition override_typeexpr_post fn (vmt : vmt_t) : vmt_t :=
      let (decl_pre, decl_post, clsdecl_pre, clsdecl_post, clsinherit_pre, clsinherit_post, funbody_pre, funbody_post, cinit_pre, cinit_post, idexpr_pre, idexpr_post, scope_pre, scope_post, templateid_pre, templateid_post, tplformarg_pre, tplformarg_post, tplarg_pre, tplarg_post, typeexpr_pre, typeexpr_post, funtypeexpr_pre, funtypeexpr_post, funarg_pre, funarg_post, stmt_pre, stmt_post, condition_pre, condition_post, expr_pre, expr_post, binder_pre, binder_post) := vmt in
      let typeexpr_post := fn in
      vmt_make decl_pre decl_post clsdecl_pre clsdecl_post clsinherit_pre clsinherit_post funbody_pre funbody_post cinit_pre cinit_post idexpr_pre idexpr_post scope_pre scope_post templateid_pre templateid_post tplformarg_pre tplformarg_post tplarg_pre tplarg_post typeexpr_pre typeexpr_post funtypeexpr_pre funtypeexpr_post funarg_pre funarg_post stmt_pre stmt_post condition_pre condition_post expr_pre expr_post binder_pre binder_post.

  End Dependent.
End visitor.

Section Dependent.
  Variable T : Set.
  Variable visitor : visitor.vmt_t T.

  Fixpoint decls_visit (state : T) (decls : decls_t) {struct decls} : T * decls_t :=
    match decls with
      | decls_nil => (state, decls_nil)
      | decls_cons decl decls =>
        let (state, decl) := decl_visit state decl in
        let (state, decls) := decls_visit state decls in
        (state, decls_cons decl decls)
    end

  with decl_visit (state : T) (decl : decl_t) {struct decl} : T * decl_t :=
    let (descend, state) := visitor.decl_pre T visitor decl state in
    let (decl, state) := if descend
      then
        match decl with
          | decl_simple ds type id attrs =>
            let (state, type) := typeexpr_visit state type in
            let (state, id) := idexpr_visit state id in
            (decl_simple ds type id attrs, state)
          | decl_initdef ds type id attrs value =>
            let (state, type) := typeexpr_visit state type in
            let (state, id) := idexpr_visit state id in
            let (state, value) := expr_visit state value in
            (decl_initdef ds type id attrs value, state)
          | decl_fundef ds type id attrs body =>
            let (state, type) := funtypeexpr_visit state type in
            let (state, id) := idexpr_visit state id in
            let (state, body) := funbody_visit state body in
            (decl_fundef ds type id attrs body, state)
          | decl_consdesdecl ds id args qual attr =>
            let (state, id) := idexpr_visit state id in
            let (state, args) := funargs_visit state args in
            (decl_consdesdecl ds id args qual attr, state)
          | decl_consdesdef ds id args qual attrs body =>
            let (state, id) := idexpr_visit state id in
            let (state, args) := funargs_visit state args in
            let (state, body) := funbody_visit state body in
            (decl_consdesdef ds id args qual attrs body, state)
          | decl_class_fwd id =>
            let (state, id) := idexpr_visit state id in
            (decl_class_fwd id, state)
          | decl_class id is_final inherits body =>
            let (state, id) := idexpr_visit state id in
            let (state, inherits) := clsinherits_visit state inherits in
            let (state, body) := clsdecls_visit state body in
            (decl_class id is_final inherits body, state)
          | decl_templated args decl =>
            let (state, args) := tplformargs_visit state args in
            let (state, decl) := decl_visit state decl in
            (decl_templated args decl, state)
        end
      else (decl, state) in
    let (decl, state) := visitor.decl_post T visitor decl state in
    (state, decl)

  with clsdecls_visit (state : T) (clsdecls : clsdecls_t) {struct clsdecls} : T * clsdecls_t :=
    match clsdecls with
      | clsdecls_nil => (state, clsdecls_nil)
      | clsdecls_cons clsdecl clsdecls =>
        let (state, clsdecl) := clsdecl_visit state clsdecl in
        let (state, clsdecls) := clsdecls_visit state clsdecls in
        (state, clsdecls_cons clsdecl clsdecls)
    end

  with clsdecl_visit (state : T) (clsdecl : clsdecl_t) {struct clsdecl} : T * clsdecl_t :=
    let (descend, state) := visitor.clsdecl_pre T visitor clsdecl state in
    let (clsdecl, state) := if descend
      then
        let (visibility, decls) := clsdecl in
        let (state, decls) := decls_visit state decls in
        (clsdecl_group visibility decls, state)
      else (clsdecl, state) in
    let (clsdecl, state) := visitor.clsdecl_post T visitor clsdecl state in
    (state, clsdecl)

  with clsinherits_visit (state : T) (clsinherits : clsinherits_t) {struct clsinherits} : T * clsinherits_t :=
    match clsinherits with
      | clsinherits_nil => (state, clsinherits_nil)
      | clsinherits_cons clsinherit clsinherits =>
        let (state, clsinherit) := clsinherit_visit state clsinherit in
        let (state, clsinherits) := clsinherits_visit state clsinherits in
        (state, clsinherits_cons clsinherit clsinherits)
    end

  with clsinherit_visit (state : T) (clsinherit : clsinherit_t) {struct clsinherit} : T * clsinherit_t :=
    let (descend, state) := visitor.clsinherit_pre T visitor clsinherit state in
    let (clsinherit, state) := if descend
      then
        let (is_virtual, visibility, base) := clsinherit in
        let (state, base) := idexpr_visit state base in
        (clsinherit_single is_virtual visibility base, state)
      else (clsinherit, state) in
    let (clsinherit, state) := visitor.clsinherit_post T visitor clsinherit state in
    (state, clsinherit)

  with funbody_visit (state : T) (funbody : funbody_t) {struct funbody} : T * funbody_t :=
    let (descend, state) := visitor.funbody_pre T visitor funbody state in
    let (funbody, state) := if descend
      then
        match funbody with
          | funbody_abstract => (funbody_abstract, state)
          | funbody_default => (funbody_default, state)
          | funbody_delete => (funbody_delete, state)
          | funbody_stmts init stmts =>
            let (state, init) := cinits_visit state init in
            let (state, stmts) := stmts_visit state stmts in
            (funbody_stmts init stmts, state)
        end
      else (funbody, state) in
    let (funbody, state) := visitor.funbody_post T visitor funbody state in
    (state, funbody)

  with cinits_visit (state : T) (cinits : cinits_t) {struct cinits} : T * cinits_t :=
    match cinits with
      | cinits_nil => (state, cinits_nil)
      | cinits_cons cinit cinits =>
        let (state, cinit) := cinit_visit state cinit in
        let (state, cinits) := cinits_visit state cinits in
        (state, cinits_cons cinit cinits)
    end

  with cinit_visit (state : T) (cinit : cinit_t) {struct cinit} : T * cinit_t :=
    let (descend, state) := visitor.cinit_pre T visitor cinit state in
    let (cinit, state) := if descend
      then
        let (id, expr) := cinit in
        let (state, expr) := expr_visit state expr in
        (cinit_make id expr, state)
      else (cinit, state) in
    let (cinit, state) := visitor.cinit_post T visitor cinit state in
    (state, cinit)

  with idexpr_visit (state : T) (idexpr : idexpr_t) {struct idexpr} : T * idexpr_t :=
    let (descend, state) := visitor.idexpr_pre T visitor idexpr state in
    let (idexpr, state) := if descend
      then
        match idexpr with
          | idexpr_id scope id =>
            let (state, scope) := scope_visit state scope in
            (idexpr_id scope id, state)
          | idexpr_destructor scope id =>
            let (state, scope) := scope_visit state scope in
            (idexpr_destructor scope id, state)
          | idexpr_template scope tplid =>
            let (state, scope) := scope_visit state scope in
            let (state, tplid) := templateid_visit state tplid in
            (idexpr_template scope tplid, state)
          | idexpr_operator scope op =>
            let (state, scope) := scope_visit state scope in
            (idexpr_operator scope op, state)
        end
      else (idexpr, state) in
    let (idexpr, state) := visitor.idexpr_post T visitor idexpr state in
    (state, idexpr)

  with scope_visit (state : T) (scope : scope_t) {struct scope} : T * scope_t :=
    let (descend, state) := visitor.scope_pre T visitor scope state in
    let (scope, state) := if descend
      then
        match scope with
          | scope_none =>
            (scope_none, state)
          | scope_id id sub =>
            let (state, sub) := scope_visit state sub in
            (scope_id id sub, state)
          | scope_template tplid sub =>
            let (state, tplid) := templateid_visit state tplid in
            let (state, sub) := scope_visit state sub in
            (scope_template tplid sub, state)
        end
      else (scope, state) in
    let (scope, state) := visitor.scope_post T visitor scope state in
    (state, scope)

  with templateid_visit (state : T) (templateid : templateid_t) {struct templateid} : T * templateid_t :=
    let (descend, state) := visitor.templateid_pre T visitor templateid state in
    let (templateid, state) := if descend
      then
        let (id, tplargs) := templateid in
        let (state, tplargs) := tplargs_visit state tplargs in
        (templateid_make id tplargs, state)
      else (templateid, state) in
    let (templateid, state) := visitor.templateid_post T visitor templateid state in
    (state, templateid)

  with tplformargs_visit (state : T) (tplformargs : tplformargs_t) {struct tplformargs} : T * tplformargs_t :=
    match tplformargs with
      | tplformargs_nil => (state, tplformargs_nil)
      | tplformargs_cons tplformarg tplformargs =>
        let (state, tplformarg) := tplformarg_visit state tplformarg in
        let (state, tplformargs) := tplformargs_visit state tplformargs in
        (state, tplformargs_cons tplformarg tplformargs)
    end

  with tplformarg_visit (state : T) (tplformarg : tplformarg_t) {struct tplformarg} : T * tplformarg_t :=
    let (descend, state) := visitor.tplformarg_pre T visitor tplformarg state in
    let (tplformarg, state) := if descend
      then
        match tplformarg with
          | tplformarg_typename id =>
            (tplformarg_typename id, state)
          | tplformarg_value type id =>
            let (state, type) := typeexpr_visit state type in
            (tplformarg_value type id, state)
        end
      else (tplformarg, state) in
    let (tplformarg, state) := visitor.tplformarg_post T visitor tplformarg state in
    (state, tplformarg)

  with tplargs_visit (state : T) (tplargs : tplargs_t) {struct tplargs} : T * tplargs_t :=
    match tplargs with
      | tplargs_nil => (state, tplargs_nil)
      | tplargs_cons tplarg tplargs =>
        let (state, tplarg) := tplarg_visit state tplarg in
        let (state, tplargs) := tplargs_visit state tplargs in
        (state, tplargs_cons tplarg tplargs)
    end

  with tplarg_visit (state : T) (tplarg : tplarg_t) {struct tplarg} : T * tplarg_t :=
    let (descend, state) := visitor.tplarg_pre T visitor tplarg state in
    let (tplarg, state) := if descend
      then
        match tplarg with
          | tplarg_type type =>
            let (state, type) := typeexpr_visit state type in
            (tplarg_type type, state)
          | tplarg_expr expr =>
            let (state, expr) := expr_visit state expr in
            (tplarg_expr expr, state)
        end
      else (tplarg, state) in
    let (tplarg, state) := visitor.tplarg_post T visitor tplarg state in
    (state, tplarg)

  with typeexpr_visit (state : T) (typeexpr : typeexpr_t) {struct typeexpr} : T * typeexpr_t :=
    let (descend, state) := visitor.typeexpr_pre T visitor typeexpr state in
    let (typeexpr, state) := if descend
      then
        match typeexpr with
          | typeexpr_primitive prim =>
            (typeexpr_primitive prim, state)
          | typeexpr_id id =>
            let (state, id) := idexpr_visit state id in
            (typeexpr_id id, state)
          | typeexpr_const type =>
            let (state, type) := typeexpr_visit state type in
            (typeexpr_const type, state)
          | typeexpr_volatile type =>
            let (state, type) := typeexpr_visit state type in
            (typeexpr_volatile type, state)
          | typeexpr_pointer type =>
            let (state, type) := typeexpr_visit state type in
            (typeexpr_pointer type, state)
          | typeexpr_reference type =>
            let (state, type) := typeexpr_visit state type in
            (typeexpr_reference type, state)
          | typeexpr_rvaluereference type =>
            let (state, type) := typeexpr_visit state type in
            (typeexpr_rvaluereference type, state)
          | typeexpr_array type size =>
            let (state, type) := typeexpr_visit state type in
            let (state, size) := expr_visit state size in
            (typeexpr_array type size, state)
          | typeexpr_unspec_array type =>
            let (state, type) := typeexpr_visit state type in
            (typeexpr_unspec_array type, state)
          | typeexpr_function type =>
            let (state, type) := funtypeexpr_visit state type in
            (typeexpr_function type, state)
          | typeexpr_decltype expr =>
            let (state, expr) := expr_visit state expr in
            (typeexpr_decltype expr, state)
        end
      else (typeexpr, state) in
    let (typeexpr, state) := visitor.typeexpr_post T visitor typeexpr state in
    (state, typeexpr)

  with funtypeexpr_visit (state : T) (funtypeexpr : funtypeexpr_t) {struct funtypeexpr} : T * funtypeexpr_t :=
    let (descend, state) := visitor.funtypeexpr_pre T visitor funtypeexpr state in
    let (funtypeexpr, state) := if descend
      then
        match funtypeexpr with
          | funtypeexpr_make ret_type args qual postfix =>
            let (state, ret_type) := typeexpr_visit state ret_type in
            let (state, args) := funargs_visit state args in
            (funtypeexpr_make ret_type args qual postfix, state)
        end
      else (funtypeexpr, state) in
    let (funtypeexpr, state) := visitor.funtypeexpr_post T visitor funtypeexpr state in
    (state, funtypeexpr)

  with funargs_visit (state : T) (funargs : funargs_t) {struct funargs} : T * funargs_t :=
    match funargs with
      | funargs_nil => (state, funargs_nil)
      | funargs_cons funarg funargs =>
        let (state, funarg) := funarg_visit state funarg in
        let (state, funargs) := funargs_visit state funargs in
        (state, funargs_cons funarg funargs)
    end

  with funarg_visit (state : T) (funarg : funarg_t) {struct funarg} : T * funarg_t :=
    let (descend, state) := visitor.funarg_pre T visitor funarg state in
    let (funarg, state) := if descend
      then
        match funarg with
          | funarg_named type name =>
            let (state, type) := typeexpr_visit state type in
            (funarg_named type name, state)
          | funarg_anon type =>
            let (state, type) := typeexpr_visit state type in
            (funarg_anon type, state)
        end
      else (funarg, state) in
    let (funarg, state) := visitor.funarg_post T visitor funarg state in
    (state, funarg)

  with stmts_visit (state : T) (stmts : stmts_t) {struct stmts} :
      T * stmts_t :=
    match stmts with
      | stmts_nil => (state, stmts_nil)
      | stmts_cons stmt stmts =>
        let (state, stmt) := stmt_visit state stmt in
        let (state, stmts) := stmts_visit state stmts in
        (state, stmts_cons stmt stmts)
    end

  with stmt_visit (state : T) (stmt : stmt_t) {struct stmt} : T * stmt_t :=
    let (descend, state) := visitor.stmt_pre T visitor stmt state in
    let (stmt, state) := if descend
      then
        match stmt with
          | stmt_decl decl =>
            let (state, decl) := decl_visit state decl in
            (stmt_decl decl, state)
          | stmt_expr expr =>
            let (state, expr) := expr_visit state expr in
            (stmt_expr expr, state)
          | stmt_block body =>
            let (state, body) := stmts_visit state body in
            (stmt_block body, state)
          | stmt_if cond then_body =>
            let (state, cond) := condition_visit state cond in
            let (state, then_body) := stmt_visit state then_body in
            (stmt_if cond then_body, state)
          | stmt_ifelse cond then_body else_body =>
            let (state, cond) := condition_visit state cond in
            let (state, then_body) := stmt_visit state then_body in
            let (state, else_body) := stmt_visit state else_body in
            (stmt_ifelse cond then_body else_body, state)
          | stmt_while cond body =>
            let (state, cond) := condition_visit state cond in
            let (state, body) := stmt_visit state body in
            (stmt_while cond body, state)
          | stmt_do_while body cond =>
            let (state, body) := stmt_visit state body in
            let (state, cond) := condition_visit state cond in
            (stmt_do_while body cond, state)
          | stmt_for init cond cont body =>
            let (state, init) := condition_visit state init in
            let (state, cond) := condition_visit state cond in
            let (state, cont) := expr_visit state cont in
            let (state, body) := stmt_visit state body in
            (stmt_for init cond cont body, state)
          | stmt_for_range type id range body =>
            let (state, type) := typeexpr_visit state type in
            let (state, id) := idexpr_visit state id in
            let (state, range) := expr_visit state range in
            let (state, body) := stmt_visit state body in
            (stmt_for_range type id range body, state)
          | stmt_return expr =>
            let (state, expr) := expr_visit state expr in
            (stmt_return expr, state)
        end
      else (stmt, state) in
    let (stmt, state) := visitor.stmt_post T visitor stmt state in
    (state, stmt)

  with condition_visit (state : T) (condition : condition_t) {struct condition} : T * condition_t :=
    let (descend, state) := visitor.condition_pre T visitor condition state in
    let (condition, state) := if descend
      then
        match condition with
          | condition_expr expr =>
            let (state, expr) := expr_visit state expr in
            (condition_expr expr, state)
          | condition_decl ds type id attrs value =>
            let (state, type) := typeexpr_visit state type in
            let (state, id) := idexpr_visit state id in
            let (state, value) := expr_visit state value in
            (condition_decl ds type id attrs value, state)
        end
      else (condition, state) in
    let (condition, state) := visitor.condition_post T visitor condition state in
    (state, condition)

  with expr_visit (state : T) (expr : expr_t) {struct expr} : T * expr_t :=
    let (descend, state) := visitor.expr_pre T visitor expr state in
    let (expr, state) := if descend
      then
        match expr with
          | expr_id id =>
            let (state, id) := idexpr_visit state id in
            (expr_id id, state)
          | expr_literal value =>
            (expr_literal value, state)
          | expr_unop op arg =>
            let (state, arg) := expr_visit state arg in
            (expr_unop op arg, state)
          | expr_binop op arg1 arg2 =>
            let (state, arg1) := expr_visit state arg1 in
            let (state, arg2) := expr_visit state arg2 in
            (expr_binop op arg1 arg2, state)
          | expr_ternop cond then_expr else_expr =>
            let (state, cond) := expr_visit state cond in
            let (state, then_expr) := expr_visit state then_expr in
            let (state, else_expr) := expr_visit state else_expr in
            (expr_ternop cond then_expr else_expr, state)
          | expr_call fn args =>
            let (state, fn) := expr_visit state fn in
            let (state, args) := callargs_visit state args in
            (expr_call fn args, state)
          | expr_lambda binders args body =>
            let (state, binders) := binders_visit state binders in
            let (state, args) := funargs_visit state args in
            let (state, body) := stmts_visit state body in
            (expr_lambda binders args body, state)
          | expr_dynamic_cast target_type value =>
            let (state, target_type) := typeexpr_visit state target_type in
            let (state, value) := expr_visit state value in
            (expr_dynamic_cast target_type value, state)
          | expr_memdot structure member =>
            let (state, structure) := expr_visit state structure in
            let (state, member) := idexpr_visit state member in
            (expr_memdot structure member, state)
          | expr_memarrow structure member =>
            let (state, structure) := expr_visit state structure in
            let (state, member) := idexpr_visit state member in
            (expr_memarrow structure member, state)
          | expr_throw expr =>
            let (state, expr) := expr_visit state expr in
            (expr_throw expr, state)
        end
      else (expr, state) in
    let (expr, state) := visitor.expr_post T visitor expr state in
    (state, expr)

  with callargs_visit (state : T) (callargs : callargs_t) {struct callargs} :
      T * callargs_t :=
    match callargs with
      | callargs_nil => (state, callargs_nil)
      | callargs_cons arg callargs =>
        let (state, arg) := expr_visit state arg in
        let (state, callargs) := callargs_visit state callargs in
        (state, callargs_cons arg callargs)
    end

  with binders_visit (state : T) (binders : binders_t) {struct binders} :
      T * binders_t :=
    match binders with
      | binders_nil => (state, binders_nil)
      | binders_cons binder binders =>
        let (state, binder) := binder_visit state binder in
        let (state, binders) := binders_visit state binders in
        (state, binders_cons binder binders)
    end

  with binder_visit (state : T) (binder : binder_t) {struct binder} : T * binder_t :=
    let (descend, state) := visitor.binder_pre T visitor binder state in
    let (binder, state) := if descend
      then
        match binder with
          | binder_allcopy =>
            (binder_allcopy, state)
          | binder_allref =>
            (binder_allref, state)
          | binder_copy id =>
            (binder_copy id, state)
          | binder_ref id =>
            (binder_ref id, state)
          | binder_generic id expr =>
            let (state, expr) := expr_visit state expr in
            (binder_generic id expr, state)
        end
      else (binder, state) in
    let (binder, state) := visitor.binder_post T visitor binder state in
    (state, binder).

End Dependent.
