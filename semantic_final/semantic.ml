open Printf
open Ast


exception Error of string

type dfa_table = 
{
    dfas: (datatype * ident * formal list * sstmt list * snode list) list
} 

type symbol_table = 
{
    variables: (ident * datatype * value option) list;
    parent: symbol_table option;
}  

type translation_environment = 
{
    dfa_lookup: dfa_table;    (* defined above*)
    node_scope: symbol_table; (* defined above*)
    return_seen: bool;        (* bool type *)
    return_type: datatype;    (* int, float, string or stacktype*)
    location: string;
}

let find_dfa (dfa_lookup: dfa_table) name =
    List.find (fun (_,s,_,_,_) -> s=name) dfa_lookup.dfas

let get_ident_name ident = match ident with
    Ident(n) -> n

let rec get_type_from_datatype = function
    | Datatype(t)->t
    | Stacktype(ty) -> get_type_from_datatype ty
    | Eostype(t) -> Void

let math_logic x y = match (x, y) with     (* math logic =, !=, etc*)
    | (String,String) -> (Int,true)        
    | (Int,Int) -> (Int,true)               
    | (Int,Float) -> (Int,true)
    | (Float,Float) -> (Int,true)
    | (Float,Int) -> (Int,true)
    | (_,_) -> (Int,false) 

let math_sym x y = match (x, y) with       (* math symbols >,<,=>,<= etc*)
    | (Int,Int) -> (Int,true) 
    | (Int,Float) -> (Int,true)
    | (Float,Int) -> (Int,true)
    | (Float,Float) -> (Int,true)
    | (_,_) -> (Int, false) 

let math_operators x y = match (x, y) with  (* math logic =, !=, etc*)
    | (String, String) -> (String, true)
    | (Int, Int) -> (Int, true)
    | (Int, Float) -> (Float, true)
    | (Float, Float) -> (Int, true)
    | (Float, Int) -> (Float, true)
    | (_,_) -> (Int, false)


let get_binop_return_value operator p q = 
  let x = get_type_from_datatype p and
    y = get_type_from_datatype q in
    let (t, valid) = 
        match operator with 
            | Neq -> math_logic x y
            | Equal -> math_logic x y
            | Mult -> math_operators x y
            | Div -> math_operators x y
            | Lt -> math_sym x y 
            | Leq -> math_sym x y
            | Gt -> math_sym x y
            | Add -> math_operators x y
            | Sub -> math_operators x y
            | Geq -> math_sym x y
            | And -> math_sym x y
            | Or -> math_sym x y
            | Mod -> math_operators x y

        in (Datatype(t), valid) 

        

let update_variable env (temp, dt, value) = (* semantic check for scope errors*)
    let ((_,_,_), location) = 
    try (fun node_scope -> ((List.find (fun (s,_,_) -> s = temp ) node_scope),1)) env.node_scope.variables
        with 
        Not_found ->
        try
        let globalScope = match env.node_scope.parent with
        Some scope -> scope
        | None -> raise(Error("No Global Scope - 1"))  
        in  
        (fun node_scope -> ((List.find (fun (s,_,_) -> s = temp)
        node_scope),2)) globalScope.variables
            with Not_found -> raise(Error("Not Found exception in update_variable"))in
    let new_envf =
    match location with 
        | 1 ->                       (* Variables in the node *)               
            let new_vars = List.map (fun (n, t, v) -> if(n = temp) then (temp, dt, value) else (n, t, v)) env.node_scope.variables in
            let new_sym_table = {parent = env.node_scope.parent; variables = new_vars;} in
            let new_env = {env with node_scope = new_sym_table} in
            new_env
        
        | 2 ->                       (* Variables in the DFA *) 
            let globalScope = match env.node_scope.parent with
              Some scope -> scope
            | None -> raise(Error("No Global Scope - 2"))  
            in
            let new_vars = 
            List.map (fun (n, t, v) -> if(n = temp) then (temp, dt, value) else (n, t, v)) globalScope.variables in
            let new_dfa_sym_table = 
            { parent = None; 
                    variables = new_vars;} in
            let new_node_scope = 
            { env.node_scope with parent = Some(new_dfa_sym_table);} in
            
            let new_env = 
            { env with node_scope = new_node_scope} in
            new_env
        
        | _ -> raise(Error("Undefined scope"))  (* scope Not defined *)
    in new_envf

let find_local_variable env temp =  (* find the local variable *)
    List.find (fun (s,_,_) -> s = temp) env.node_scope.variables

let get_name_type_from_formal env = function
    Formal(datatype,ident) -> (ident,datatype,None)

let find_variable env temp =  (*find the variable*)
  try List.find (fun(s,_,_) -> s = temp) env.node_scope.variables
  with Not_found ->  
    let globalScope = (match env.node_scope.parent with
      Some scope -> scope
      |None -> raise(Error("No Global Scope - 3")))  (*global scope not present*)
      in List.find(fun (s,_,_) -> s=temp) globalScope.variables



let rec check_expr env exp =                 (*check the given expression compatible or not*)
    match exp with
    | StringLit(s) -> Datatype(String)
    | IntLit(i) ->Datatype(Int)
    
    | FloatLit(f) -> Datatype(Float)
    
    | EosLit -> Eostype(Eos)
    
    | Variable(v) ->  let (_,s_type,_) = try find_variable env v with 
            Not_found ->
                raise (Error("Undeclared Identifier ")); in s_type    (*If there is an undefined id used in the code *)
    | Unop(u, exp) -> let t = check_expr env exp in 
        (match u with
             _ -> if t = Datatype(Int) then t else if t = Datatype(Float) then t 
                        else
                            raise (Error("Cannot perform operation on " )))  (* operation can not be performed*)
    
    | BinaryOp(e1, b, e2) -> let p = check_expr env e1 and q = check_expr env e2 in 
        let (t, valid) = get_binop_return_value b p q in
        if valid || e1 = EosLit || e2 = EosLit 
        then t else raise(Error("Incompatible types with binary operator"));  (*binary operations are not compatible*)
    
    | Peek(id) -> let (_,p,_) = (find_variable env id) in p        (*peek operation*)
    
    | Push(id, exp) -> let (_,p,_) = (find_variable env id) and q = (*push operation*)
        check_expr env exp 
    in (if not (p = Stacktype(q)) then (raise (Error("Mismatch in types for assignment")))); q  (*error in the stack operations*)
    
    | Pop(id) -> let (_,p,_) = (find_variable env id) in p         (*pop operation*)
    
    | Call(Ident("concurrent"), e_list) -> 
            let dfaArgsList =  List.filter( function Call(_,_) -> false
            | _ -> true) e_list 
        in
        if dfaArgsList != [] then raise(Error("Not all arguments passed to concurrent are dfas")) else Stacktype(Datatype(String))
    
    | Call(id, e) -> try (let (dfa_ret, dfa_name, dfa_args, dfa_var_body, dfa_node_body)  = find_dfa env.dfa_lookup id in
                let el_tys = List.map (fun exp -> check_expr env exp) e in
                let fn_tys = List.map (fun dfa_arg-> let (_,ty,_) =
                  get_name_type_from_formal env dfa_arg in ty) dfa_args in
                
                if ( id = Ident("print") || id = Ident("state")  || id = Ident("input") || id = Ident("concurrent" ) ) 
                then dfa_ret
                else
                  if not (el_tys = fn_tys) then
                      raise (Error("Mismatching types in function call")) 
                  else
                      dfa_ret)
            with Not_found ->
                raise (Error("Undeclared Function: " ^ get_ident_name id))

let get_node_scope env temp = 
  if env.location = "dfa" then DFAScope
  else
    try (let (_,_,_) = List.find (fun (s,_,_) -> s = temp) env.node_scope.variables in NodeScope)
    with Not_found -> let globalScope = (match env.node_scope.parent with
        Some scope -> scope
        |None -> raise(Error("No Global Scope - 4")))
    in try (let (_,_,_) = List.find(fun (s,_,_) -> s = temp) globalScope.variables in DFAScope)
    with Not_found -> raise(Error("get_node_scope is failing"))

let rec get_sexpr env exp = 
    match exp with
      | Peek(id) -> SPeek(SIdent(id, get_node_scope env id),
        check_expr env exp)
      | Push(id, ex) -> SPush(SIdent(id, get_node_scope env id), 
        get_sexpr env ex,check_expr env exp)
      | Pop(id) -> SPop(SIdent(id, get_node_scope env id), 
        check_expr env exp)

      | StringLit(s) -> SStringLit(s,Datatype(String))
      | IntLit(i) -> SIntLit(i, Datatype(Int))
      | FloatLit(d) -> SFloatLit(d,Datatype(Float))

      | Call(id, ex_list) -> let s_ex_list = List.map(fun exp -> get_sexpr env exp) ex_list in 
        SCall(SIdent(id,StateScope),s_ex_list, check_expr env exp)
      | Variable(id) -> SVariable(SIdent(id, get_node_scope env id), check_expr env exp)
      | BinaryOp(e1,b,e2) -> SBinop(get_sexpr env e1,b, get_sexpr env e2,check_expr env exp)
      | Unop(u,ex) -> SUnop(u, get_sexpr env ex, check_expr env exp) 

      | EosLit -> SEosLit

let get_sval env = function
    ExprVal(expr) -> SExprVal(get_sexpr env expr)

let get_sdecl env decl =
  let scope = match env.node_scope.parent with
    | Some(_) -> NodeScope
    |None -> DFAScope
  in match decl with
    | VarDecl(datatype, ident) -> (SVarDecl(datatype, SIdent(ident, scope)), env)
    | VarAssignDecl(datatype, ident, value) -> let sv = get_sval env value in
        (SVarAssignDecl(datatype, SIdent(ident, scope), sv), env)


let get_name_type_val_from_decl temp = 
    match temp with
    | VarDecl(datatype, ident) -> (ident, datatype, None)
    | VarAssignDecl(datatype, ident, value) -> (ident, datatype, Some(value))

let get_name_type_from_decl temp = 
    match temp with
    | VarDecl(datatype, ident) -> (ident, datatype)
    | VarAssignDecl(datatype,ident,value) -> (ident,datatype)

let get_name_type_from_var env = function
    | VarDecl(datatype,ident) -> (ident,datatype,None)
    | VarAssignDecl(datatype,ident,value) -> (ident,datatype,Some(value))

let get_datatype_from_val env = function
    ExprVal(expr) -> check_expr env expr
 

let check_assignments type1 type2 = match (type1, type2) with
    (Int, Int) -> true
    |(Float, Float) -> true
    |(Int, Float) -> true
    |(Float, Int) -> true
    |(String, String) -> true
    |(_,_) -> false

let add_to_var_table env name t v = 
    
    let new_vars = (name,t, v)::env.node_scope.variables in
    
    let new_sym_table = { variables = new_vars; 
                            parent = env.node_scope.parent; } in
    let new_env = {env with node_scope = new_sym_table} in
    new_env


let check_final_env env =
    (if(false = env.return_seen && env.return_type <> Datatype(Void)) then
        raise (Error("Missing Return Statement")));
    
    true

let match_var_type env v t =
    let(name,ty,value) = find_variable env v in
    if(t<>ty) then false else true

    
    (* defined names and thier initialization *)
let empty_table_initialization = {parent=None; variables =[];}
let empty_dfa_table_initialization = {
    dfas=[
        (Datatype(String), Ident("input"),[],[],[]);                    (*The built-in get-user-input function*)
    
        (Datatype(String), Ident("state"),                              (*The built-in 'get state' function for concurrently running dfas *)
        [Formal(Datatype(String),Ident("dfa"))],[],[]);

        (Datatype(Void), Ident("print"),                                (*The built-in print function (only prints strings)*)
        [Formal(Datatype(String),Ident("str"))],[], []);
    
        (Datatype(String), Ident("state"),                              (*The state() function to get states of concurrently running dfas*)
        [Formal(Datatype(String),Ident("dfa"))],[], []);
    
        (Stacktype(Datatype(String)), Ident("concurrent"), [] ,[], [])  (* The built-in concurrent stringhow to check formals*)
    ]}

let empty_environment = {return_type = Datatype(Void); return_seen = false;
    location="in_dfa"; 
    node_scope = {empty_table_initialization with parent =
      Some(empty_table_initialization)}; 
      dfa_lookup = empty_dfa_table_initialization}

let find_global_variable env name =
  let globalScope = match env.node_scope.parent with
  Some scope -> scope 
    
    | None -> raise (Error("No global scope")) in
    try List.find (fun (s,_,_) -> s=name) globalScope.variables
    with Not_found -> raise (Error("error in find_global_variable"))

let rec check_stmt env stmt = match stmt with
    
    | Return(e) ->
        let type1=check_expr env e in
        
        if env.return_type <> Datatype(Void) && type1 <> env.return_type then
            raise (Error("Incompatible Return Type"));  (*if the return wrote is wrong, incompaitable*)
        
        let new_env = {env with return_seen=true} in
        (SReturn(get_sexpr env e), new_env)

    | Block(stmt_list) ->
        let new_env=env in
        
        let getter(env,acc) s =
            let (st, ne) = check_stmt env s in
            (ne, st::acc) in
        let (ls,st) = List.fold_left(fun e s ->
            getter e s) (new_env,[]) stmt_list in
        
        let revst = List.rev st in (SBlock(revst),ls)

    | Expr(e) -> 
        let _ = check_expr env e in
        (SSExpr(get_sexpr env e),env)

    | Transition(idState,ex) ->
       let t=get_type_from_datatype(check_expr env ex) in
       if not(t=Int) then
           raise(Error("Improper Transition Expression Datatype")) else
       (STransition(SIdent(idState, StateScope), get_sexpr env ex), env)

    | Ast.Assign(ident, expr) ->
        let (_, dt, _) = try find_variable env ident with Not_found -> raise (Error("Uninitialized variable")) in
        
        let t1 = get_type_from_datatype dt 
        and t2 = get_type_from_datatype(check_expr env expr) in
       
        if( not(t1=t2) ) then 
            raise (Error("Mismatched type assignments"));
        
        let sexpr = get_sexpr env expr in
        
        let new_env = update_variable env (ident,dt,Some((ExprVal(expr)))) in
        (SAssign(SIdent(ident, get_node_scope env ident), sexpr), new_env)

    | Ast.Declaration(decl) -> 
       
        let (name, ty) = get_name_type_from_decl decl in
        
        let ((_,dt,_),found) = try (fun f -> ((f env name),true)) find_local_variable with 
            Not_found ->
                ((name,ty,None),false) in
       
        let ret = if(found=false) then
            match decl with
                VarDecl(_,_) ->
                    let (sdecl,_) = get_sdecl env decl in
                    
                    let (n, t, v) = get_name_type_val_from_decl decl in
                    
                    let new_env = add_to_var_table env n t v in
                    (SDeclaration(sdecl), new_env)
                | VarAssignDecl(dt, id, value) ->
                    
                    let t1 = get_type_from_datatype(dt) and t2 = get_type_from_datatype(get_datatype_from_val env value) in
                    if(t1=t2) then
                        
                        let (sdecl,_) = get_sdecl env decl in
                        
                        let (n, t, v) = get_name_type_val_from_decl decl in
                        
                        let new_env = add_to_var_table env n t v in
                        (SDeclaration(sdecl), new_env)
                    else raise (Error("Type mismatch"))
                else
                    raise (Error("Multiple declarations")) in ret  (*If they declared multiple times*)
    
let get_svar_list env var_list = 
     List.fold_left (fun (svar_list,env) var -> 
        
        let stmt = match var with
        decl -> Ast.Declaration(var)
        in
       
        let (svar, new_env) = check_stmt env stmt in 
        (svar::svar_list, new_env)) ([],env) var_list

let get_sstmt_list env stmt_list = 
     List.fold_left (fun (sstmt_list,env) stmt -> 
        
        let (sstmt, new_env) = check_stmt env stmt in 
        (sstmt::sstmt_list, new_env)) ([],env) stmt_list

let add_dfa env sdfa_decl =
   
    let dfa_table = env.dfa_lookup in
   
    let old_dfas = dfa_table.dfas in
    match sdfa_decl with
        SDfa_Decl(sdfastr, datatype) ->
            let dfa_name = sdfastr.sdfaname in
            
            let dfa_type = get_type_from_datatype sdfastr.sreturn in
           
            let dfa_formals = sdfastr.sformals in
            
            let dfa_var_body = sdfastr.svar_body in
           
            let dfa_node_body = sdfastr.snode_body in
           
            let new_dfas = (Datatype(dfa_type), dfa_name, dfa_formals,
            dfa_var_body, dfa_node_body)::old_dfas in
           
            let new_dfa_lookup = {dfas = new_dfas} in
           
            let final_env = {env with dfa_lookup = new_dfa_lookup} in
            final_env

let get_snodeBody env node_list =
    List.fold_left (fun (snode_list, dfa_env) raw_node ->
     
      let node_sym_tab = {parent = Some(dfa_env.node_scope); variables = [];} in 
    
      let node_env = {dfa_env with node_scope = node_sym_tab;} in 
  match raw_node with
        Node((Ident(name), node_stmt_list)) ->
            let transCatchAllList = List.filter( function Transition(_,IntLit(1)) -> true
                | _ -> false) node_stmt_list in
            
            let transList = List.filter( function Transition(_,_) -> true
                | _ -> false) node_stmt_list in
           
            let retList = List.filter (function Return(_) -> true
                | _ -> false) node_stmt_list in
            
            if retList != [] && transList != [] then
              raise(Error("Return statements and Transitions are mutually exclusive"))  (**)
            else
              let block = 
                  let node_block = Block(node_stmt_list) in
                  let (snode_block, new_node_env) = check_stmt node_env node_block in
                  let new_dfa_node_scope = (match new_node_env.node_scope.parent
                  with
                  Some(scope) -> scope
                
                | None-> raise(Error("Snode check returns no dfa scope")))
                  in
                  let new_dfa_env = {dfa_env with node_scope =
                    new_dfa_node_scope; 
                    return_seen = new_node_env.return_seen} in
                  
                  (SNode(SIdent(Ident(name), NodeScope), snode_block)::snode_list, new_dfa_env) in
             
              if retList == [] then
                
                if transCatchAllList != [] then
                  block
                else raise(Error("No catch all"))(*error prints no catch at all*)

              else
                block
  ) ([],env) node_list


let check_for_Start node_list =   (*The first transition must be Start or else semantic error*)
  let allNodes = List.fold_left (fun (name_list) raw_node ->
    match raw_node with
        Node((Ident(name), node_stmt_list)) ->
            name::name_list) ([]) node_list
  in if List.mem "Start" allNodes = false then raise(Error("No Start state in node"))

let transition_check node_list =  (*semantic check for all the other transitions*)
  let allNodes = List.fold_left (fun (name_list) raw_node ->
      match raw_node with
          Node((Ident(name), node_stmt_list)) ->
              name::name_list) ([]) node_list
  in let statements = List.map (fun raw_node ->
    match raw_node with
      Node((Ident(name), node_stmt_list)) -> 
        List.map (fun x -> x) node_stmt_list) node_list

  in let flat = List.flatten statements
  in let states = List.fold_left (fun (states_list) stmt ->
    match stmt with
     
      Transition(Ident(id), ex) -> id::states_list
      | _ -> []) ([]) flat
  in List.map (fun id -> try (List.mem id allNodes) with Not_found ->
    raise(Error("Invalid state transition"))) states  (*It prints invalid state if the transition does not follow all the rules*)


let check_main env str =    (*check whether dfa main name is proper or not*)
  let id = Ident(str) in
    
    let (dt, _, _, _, _) = try(find_dfa env.dfa_lookup id)
    
    with Not_found -> raise(Error("Need DFA called main")) in  (*If there is any error in defining the name of function*)
    if dt <> Datatype(Void) then
      raise(Error("main DFA needs void return type"))  (*If there is an error in the return type of the function it prints this statement*)

let check_dfa env dfa_declaration =   (*semantic check on the dfa declaration *)
   
    try(let (_,_,_,_,_) =  find_dfa env.dfa_lookup dfa_declaration.dfa_name in 
  raise(Error("DFA already declared"))) with         (*prints it if the dfa is aldready declared*)
  
  Not_found ->
      
      let dfaFormals = List.fold_left(fun a vs -> (get_name_type_from_formal env vs)::a)[] dfa_declaration.formals in
      
      let dfa_env = {return_type = dfa_declaration.return; return_seen = false;
      location = "dfa"; node_scope = { parent = None; 
                                variables = dfaFormals;};
      dfa_lookup = env.dfa_lookup} in
      
      let _ = check_for_Start dfa_declaration.node_body in
      
      let _ = transition_check dfa_declaration.node_body in
      
      let (global_var_decls, penultimate_env) = get_svar_list dfa_env
      dfa_declaration.var_body in
      
      let location_change_env = {penultimate_env with location = "node"} in
      
      let (checked_node_body, final_env) = get_snodeBody location_change_env
      dfa_declaration.node_body in
      
      let _ = check_final_env final_env in
      
      let sdfadecl = ({sreturn = dfa_declaration.return; 
        sdfaname = dfa_declaration.dfa_name; 
        sformals = dfa_declaration.formals; svar_body = global_var_decls; 
            snode_body = checked_node_body}) in
      (SDfa_Decl(sdfadecl,dfa_declaration.return), env)

let initialize_dfas env dfa_list = 
    let (typed_dfa,last_env) = List.fold_left
        (fun (sdfadecl_list,env) dfa-> let (sdfadecl, _) = check_dfa env dfa in
                                       let final_env = add_dfa env sdfadecl in
                                       (sdfadecl::sdfadecl_list, final_env))
                                       ([],env) dfa_list in (typed_dfa,last_env)

let check_program program =
    let dfas = program in
    
    let env = empty_environment in
    
    let (typed_dfas, new_env) = initialize_dfas env dfas in
    
    let (_) = check_main new_env "main" in
    Prog(typed_dfas)





