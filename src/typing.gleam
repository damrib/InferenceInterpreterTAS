import alpha
import bitable
import gleam/dict
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import ptype.{
  type Ptype, type Tfield, Polymorphic, Tapp, Tchan, Tfield, Tinteger, Tlist,
  Tobject, Tref, Tstr, Tunit, Tvar, Weak,
}
import term.{
  type Pterm, Abs, Add, App, Assign, Call, Chan, Cons, Deref, Empty, Fork, Head,
  Ife, Ifz, Integer, Let, Object, Print, Println, Rec, Recv, Ref, Send, Str, Sub,
  Tail, Unit, Var,
}

// rule for equation generation TypingEnv |- Pterm : Ptype
pub type Rule =
  #(Pterm, Ptype, TypingEnv)

pub type InferrenceError {
  GenerationError(msg: String, rule: Rule)
  UnificationError(msg: String, eq: Equation)
  GoalNotReached(msg: String)
  GoalContradiction(msg: String, rule: Equation)
}

/// The environment storing the types of the variables that are not weak types
pub type TypingEnv =
  dict.Dict(String, Ptype)

/// The weak table stores the type of the types declared as weak (For instance we may have : '_v0 associated to Tinteger)
/// It is possible to find the weak types associated to a type 
/// (For instance, if '_v0 = Tinteger and 'v1 = Tinteger, then it possible to call get_keys(Tinteger) to get ['_v0, '_v1'])
pub type WeakTable =
  bitable.Bitable(String, Ptype)

type Equation =
  #(Ptype, Ptype)

pub type Equations =
  List(Equation)

/// type of the counter used in the program
type CounterActor =
  alpha.AlphaActor

/// getting the type associated to the name var in the environment
fn get_type(env: TypingEnv, var: String) -> Result(Ptype, Nil) {
  case dict.get(env, var) {
    Ok(ty) -> Ok(ty)
    Error(Nil) -> panic as "error when getting the type"
  }
}

/// replaces the inner types Tvar(name) by Weak(name) and adds the weak type in the weakTable
/// For instance : type a -> b becomes '_a -> '_b
fn propagate_weak(weaks: WeakTable, ty: Ptype) -> #(WeakTable, Ptype) {
  case ty {
    Tvar(name) -> {
      let new_weaks = bitable.insert_key(weaks, name, Weak(name))
      #(new_weaks, Weak(name))
    }
    Tlist(t1) -> {
      let #(new_weaks, new_ty) = propagate_weak(weaks, t1)
      #(new_weaks, Tlist(new_ty))
    }
    Tref(t1) -> {
      let #(new_weaks, new_ty) = propagate_weak(weaks, t1)
      #(new_weaks, Tref(new_ty))
    }
    Tchan(t1) -> {
      let #(new_weaks, new_ty) = propagate_weak(weaks, t1)
      #(new_weaks, Tchan(new_ty))
    }
    Tapp(t1, t2) -> {
      let #(new_weaks, new_t1) = propagate_weak(weaks, t1)
      let #(new_weaks, new_t2) = propagate_weak(new_weaks, t2)
      #(new_weaks, Tapp(new_t1, new_t2))
    }
    Polymorphic(_) -> panic as "should not happen"
    _ -> #(weaks, ty)
  }
}

/// Add a non polymorphic type to the typing environment
fn add_atomic(env: TypingEnv, name: String, ty: Ptype) -> TypingEnv {
  dict.insert(env, name, ty)
}

/// Checks if a term is partial application
/// We need the typing environment to know the types of term such as Var("x")
fn is_partial_application(term: Pterm, env: TypingEnv) -> Bool {
  case term {
    App(fun, _) -> {
      // if the depth is superior to 0, then it means we define more formal argument than applied argument
      term_depth(fun, env, dict.new(), 0) > 0
    }
    _ -> False
  }
}

/// Returns the number of arrows in a type
fn type_depth(ty: Ptype) -> Int {
  case ty {
    Tapp(_, t) | Polymorphic(Tapp(_, t)) -> 1 + type_depth(t)
    _ -> 0
  }
}

/// term_depth returns the number of arguments in a term
fn term_depth(
  term: Pterm,
  type_env: TypingEnv,
  depth_env: dict.Dict(String, Int),
  cpt: Int,
) -> Int {
  case term {
    Var(name) -> {
      case dict.get(depth_env, name) {
        Ok(depth) -> depth + cpt
        // If a variable is free in the current term
        // Then we have its type in the typing evironment
        // We use this information instead to compute the depth
        _ -> {
          let assert Ok(ty) = dict.get(type_env, name)
          type_depth(ty) + cpt
        }
      }
    }
    // It is possible to define new 
    Let(name, bind, expr) -> {
      let new_env =
        dict.insert(depth_env, name, term_depth(bind, type_env, depth_env, 0))
      term_depth(expr, type_env, new_env, cpt)
    }
    Abs(name, fun) -> {
      let new_env = dict.insert(depth_env, name, 0)
      term_depth(fun, type_env, new_env, cpt + 1)
    }
    App(fun, _) -> term_depth(fun, type_env, depth_env, cpt - 1)
    Head(expr) -> term_depth(expr, type_env, depth_env, cpt)
    Cons(expr, _) -> term_depth(expr, type_env, depth_env, cpt)
    Deref(expr) -> term_depth(expr, type_env, depth_env, cpt)
    // In Ifz(cond, then, els) -> the term depth of then and els is the same otherwise the inference would not be successful
    Ifz(_, then, _) -> term_depth(then, type_env, depth_env, cpt)
    Ife(_, then, _) -> term_depth(then, type_env, depth_env, cpt)
    Rec(name, fun) -> {
      let new_env = dict.insert(depth_env, name, 0)
      term_depth(fun, type_env, new_env, cpt)
    }
    _ -> cpt
  }
}

// si on déclare qu'un terme est expansif, alors on va appliquer du polymorphisme faible
// si on a déclarer du polymorphisme faible pour un term non expansif, il n'y aura pas de changement
// lors de l'unification
pub fn is_expansive(term: Pterm, env: TypingEnv) -> Bool {
  case term {
    App(t1, t2) ->
      // we define partial apps as expasive see partial_app_expansive in tas_test.gleam
      is_partial_application(term, env)
      || is_expansive(t1, env)
      || is_expansive(t2, env)
    Let(_, _, expr) -> is_expansive(expr, env)
    // if then is expansive but els is not typing should probably not work
    Ife(_, then, els) -> is_expansive(then, env) || is_expansive(els, env)
    Ifz(_, then, els) -> is_expansive(then, env) || is_expansive(els, env)
    Ref(_) -> True
    // The type of chan is fixed after its first use
    Chan -> True
    // Recv causes side effects and has a value other than unit
    Recv(_) -> True
    // Send causes side effects and has a value other than unit
    Send(_, _) -> True
    _ -> False
  }
}

/// This function modifies the typing environment and the weak variable tables.
/// If term is expansive then all the inner types of the type var are added to the weak table.
/// If the term is not expansive and is not an Object, we add a polymorphic type to the environment.
/// If the term is not expansive and is Object, we check if the fields are expansive 
/// and update the environement or the weak tble accordingly.
/// This function is used when generating the equations of a let term.
fn add_polymorphic(
  env: TypingEnv,
  weaks: WeakTable,
  name: String,
  term: Pterm,
  var: Ptype,
) -> #(TypingEnv, WeakTable) {
  case is_expansive(term, env) {
    True -> {
      let #(new_weaks, new_ty) = propagate_weak(weaks, var)
      #(dict.insert(env, name, new_ty), new_weaks)
    }
    False -> {
      case term, var {
        Object(fields), Tobject(types) -> {
          let res_type =
            list.fold(types, #(weaks, []), fn(acc, field_ty) {
              let assert Ok(field) =
                list.find(fields, fn(field) { field.name == field_ty.name })

              case is_expansive(field.body, env) {
                True -> {
                  let #(new_acc, new_ty) = propagate_weak(acc.0, field_ty.ty)
                  #(new_acc, [Tfield(field_ty.name, new_ty), ..acc.1])
                }
                False -> #(acc.0, [
                  Tfield(field_ty.name, Polymorphic(field_ty.ty)),
                  ..acc.1
                ])
              }
            })
          #(
            dict.insert(env, name, ptype.new_object_type(res_type.1)),
            res_type.0,
          )
        }
        _, _ -> #(dict.insert(env, name, Polymorphic(var)), weaks)
      }
    }
  }
}

/// function to check whether a type is part of the inner types of another type
fn in_type(var: String, ty: Ptype) -> Bool {
  case ty {
    Tvar(v) if v == var -> True
    Tapp(ty1, ty2) -> in_type(var, ty1) || in_type(var, ty2)
    Tlist(ty) | Tref(ty) | Tchan(ty) -> in_type(var, ty)
    Tobject(fields_ty) -> {
      list.any(fields_ty, fn(field) { in_type(var, field.ty) })
    }
    _ -> False
  }
}

/// During the unification process, we replace the inner types Tvar(var) by new_ty
fn substitute_type(
  weaks: WeakTable,
  ty: Ptype,
  var: String,
  new_ty: Ptype,
) -> Ptype {
  case ty {
    Tvar(v) if v == var -> new_ty
    Tapp(ty1, ty2) -> {
      let new_ty1 = substitute_type(weaks, ty1, var, new_ty)
      let new_ty2 = substitute_type(weaks, ty2, var, new_ty)
      Tapp(new_ty1, new_ty2)
    }
    Tchan(ty1) -> substitute_type(weaks, ty1, var, new_ty) |> Tchan
    Tlist(ty1) -> substitute_type(weaks, ty1, var, new_ty) |> Tlist
    Tref(ty1) -> substitute_type(weaks, ty1, var, new_ty) |> Tref
    Weak(name) -> {
      case bitable.get_value(weaks, name) {
        Ok(res) -> res
        Error(_) -> panic as "should not happen"
      }
    }
    Tobject(types) -> {
      list.map(types, fn(field) {
        Tfield(field.name, substitute_type(weaks, field.ty, var, new_ty))
      })
      |> ptype.new_object_type
    }
    _ -> ty
  }
}

/// During the unification process, we replace the inner types Tvar(var) by new_ty in all equations passed in argument
fn substitute_type_equations(
  weaks: WeakTable,
  equations: Equations,
  var: String,
  ty: Ptype,
) -> Equations {
  let new_type = case ty {
    Tobject(methods_type) -> {
      list.fold(equations, methods_type, fn(acc, eq) {
        case eq {
          #(Tvar(name), Tobject(mtypes)) if name == var -> {
            list.append(mtypes, acc)
          }
          _ -> acc
        }
      })
      |> list.unique
      |> ptype.new_object_type
    }
    _ -> ty
  }
  fn(eq: #(Ptype, Ptype)) -> Equation {
    let new_ty1 = substitute_type(weaks, eq.0, var, new_type)
    let new_ty2 = substitute_type(weaks, eq.1, var, new_type)
    #(new_ty1, new_ty2)
  }
  |> list.map(equations, _)
}

/// If the type ty has inner weak types then replace the inner types by their weak types
/// Or simply return ty
fn weak_type_or(weaks: WeakTable, ty: Ptype) -> Ptype {
  case ty {
    Weak(name) -> {
      case bitable.get_value(weaks, name) {
        Ok(res) -> res
        Error(_) -> panic as "should not happen"
      }
    }
    Tapp(t1, t2) -> {
      let t1 = weak_type_or(weaks, t1)
      let t2 = weak_type_or(weaks, t2)
      Tapp(t1, t2)
    }
    Tchan(t1) -> weak_type_or(weaks, t1) |> Tchan
    Tlist(t1) -> weak_type_or(weaks, t1) |> Tlist
    Tref(t1) -> weak_type_or(weaks, t1) |> Tref
    Tobject(method_types) -> {
      list.map(method_types, fn(field) {
        Tfield(field.name, weak_type_or(weaks, field.ty))
      })
      |> ptype.new_object_type
    }
    _ -> ty
  }
}

fn generate_list_rules(
  acc: List(Rule),
  list: Pterm,
  elem_type: Ptype,
  env: TypingEnv,
) -> List(Rule) {
  case list {
    Cons(elem, rest) ->
      generate_list_rules(
        [#(elem, elem_type, env), ..acc],
        rest,
        elem_type,
        env,
      )
    Deref(_) | Recv(_) | App(_, _) | Call(_, _) -> [
      #(list, Tlist(elem_type), env),
      ..acc
    ]
    Empty -> acc
    _ ->
      panic as string.append(
          "generated_list_rules should only be used on Cons and Empty : ",
          term.string_of_term(list, 0),
        )
  }
}

fn generate_object_type(
  fields: List(term.Field),
  counter: CounterActor,
) -> List(Tfield) {
  let tfields = {
    use acc, term.Field(name, _) <- list.fold(fields, [])
    let new_type = alpha.new_var(counter) |> Tvar
    [Tfield(name, new_type), ..acc]
  }
  // we reverse the list so that it is in the same order as fields
  list.reverse(tfields)
}

fn generate_object_rules(
  acc: List(Rule),
  fields: List(term.Field),
  tfields: List(Tfield),
  env: TypingEnv,
) -> List(Rule) {
  case fields, tfields {
    [], [] -> acc
    [], _ | _, [] -> panic as "should not happen"
    [field, ..rest_fields], [tfield, ..rest_tfields] -> {
      let new_rule = #(field.body, tfield.ty, env)
      generate_object_rules([new_rule, ..acc], rest_fields, rest_tfields, env)
    }
  }
}

fn generate_equations(
  acc: Equations,
  rule: Rule,
  counter: CounterActor,
  weaks: WeakTable,
) -> Result(#(Equations, WeakTable), InferrenceError) {
  case rule {
    #(Var(v), ty, env) -> {
      let assert Ok(var_type) = get_type(env, v)
        as "a variable has not been typed"
      let new_equation = #(weak_type_or(weaks, var_type), ty)
      Ok(#([new_equation, ..acc], weaks))
    }
    #(App(term1, term2), ty, env) -> {
      let new_type = alpha.new_var(counter)
      let new_rule1 = #(term1, Tapp(Tvar(new_type), ty), env)
      let new_rule2 = #(term2, Tvar(new_type), env)
      use #(new_acc, new_weaks) <- result.try(generate_equations(
        acc,
        new_rule2,
        counter,
        weaks,
      ))
      generate_equations(new_acc, new_rule1, counter, new_weaks)
    }
    #(Abs(x, term), ty, env) -> {
      let new_type1 = alpha.new_var(counter) |> Tvar
      let new_type2 = alpha.new_var(counter) |> Tvar
      let new_equation = #(ty, Tapp(new_type1, new_type2))
      let new_env = add_atomic(env, x, new_type1)
      let new_rule = #(term, new_type2, new_env)
      generate_equations([new_equation, ..acc], new_rule, counter, weaks)
    }
    #(Integer(_), ty, _) -> Ok(#([#(Tinteger, ty), ..acc], weaks))
    #(Add(t1, t2), ty, env) | #(Sub(t1, t2), ty, env) -> {
      let new_rule1 = #(t1, Tinteger, env)
      let new_rule2 = #(t2, Tinteger, env)
      let new_equation = #(Tinteger, ty)
      use #(new_acc, new_weaks) <- result.try(generate_equations(
        [new_equation, ..acc],
        new_rule2,
        counter,
        weaks,
      ))
      generate_equations(new_acc, new_rule1, counter, new_weaks)
    }
    #(Cons(_, _) as l, ty, env) -> {
      let new_type = alpha.new_var(counter) |> Tvar
      let new_equation = #(ty, Tlist(new_type))
      let new_rules = generate_list_rules([], l, new_type, env)
      use #(new_acc, new_weaks), rule <- list.try_fold(new_rules, #(
        [new_equation, ..acc],
        weaks,
      ))
      generate_equations(new_acc, rule, counter, new_weaks)
    }
    #(Head(l), ty, env) -> {
      //   env |- l : Tlist(t) 
      // ---------------------- 
      //   env |- Head(l) : t 
      let new_rule = #(l, Tlist(ty), env)
      generate_equations(acc, new_rule, counter, weaks)
    }
    #(Tail(l), ty, env) -> {
      //    env |- l : t 
      // -------------------- 
      //  env |- Tail(l) : t
      let new_rule = #(l, ty, env)
      generate_equations(acc, new_rule, counter, weaks)
    }
    #(Empty, ty, _) -> {
      let new_type = alpha.new_var(counter) |> Tvar |> Tlist
      let new_equation = #(new_type, ty)
      Ok(#([new_equation, ..acc], weaks))
    }
    #(Ife(c, t, e), ty, env) -> {
      // env |- c : t0 list env |- t : t1 env |- e : t1
      // -----------------------------------------------
      //               Ife(c, t, e) : t1
      let new_type = alpha.new_var(counter) |> Tvar |> Tlist
      let new_rule_c = #(c, new_type, env)
      let new_rule_t = #(t, ty, env)
      let new_rule_e = #(e, ty, env)
      use #(new_acc, new_weaks) <- result.try(generate_equations(
        acc,
        new_rule_e,
        counter,
        weaks,
      ))
      use #(new_acc, new_weaks) <- result.try(generate_equations(
        new_acc,
        new_rule_t,
        counter,
        new_weaks,
      ))
      generate_equations(new_acc, new_rule_c, counter, new_weaks)
    }
    #(Ifz(c, t, e), ty, env) -> {
      // env |- c : t0 list env |- t : t1 env |- e : t1
      // -----------------------------------------------
      //               Ife(c, t, e) : t1
      let new_rule_c = #(c, Tinteger, env)
      let new_rule_t = #(t, ty, env)
      let new_rule_e = #(e, ty, env)
      use #(new_acc, new_weaks) <- result.try(generate_equations(
        acc,
        new_rule_e,
        counter,
        weaks,
      ))
      use #(new_acc, new_weaks) <- result.try(generate_equations(
        new_acc,
        new_rule_t,
        counter,
        new_weaks,
      ))
      generate_equations(new_acc, new_rule_c, counter, new_weaks)
    }
    #(Let(var, bind, e), ty, env) -> {
      //  env |- bind : new_type  (var, new_type).env |- e : ty 
      // --------------------------------------------------------- 
      //          env |- let var = bind in e : ty
      use #(new_weaks, new_type) <- result.try(inference_auxiliary(
        bind,
        env,
        counter,
        weaks,
      ))
      let #(new_env, new_weaks) =
        add_polymorphic(env, new_weaks, var, bind, new_type)
      let new_rule = #(e, ty, new_env)
      generate_equations(acc, new_rule, counter, new_weaks)
    }
    #(Rec(name, fun), ty, env) -> {
      let new_env = add_atomic(env, name, ty)
      let new_rule = #(fun, ty, new_env)
      generate_equations(acc, new_rule, counter, weaks)
    }
    #(Unit, ty, _) -> Ok(#([#(Tunit, ty), ..acc], weaks))
    #(Ref(t), ty, env) -> {
      //   env |- t : new_type
      // --------------------------------
      //   env |- ref t : new_type ref
      // new_equation : ty = new_type ref
      let new_type = alpha.new_var(counter) |> Tvar
      let new_equation = #(Tref(new_type), ty)
      let new_rule = #(t, new_type, env)
      generate_equations([new_equation, ..acc], new_rule, counter, weaks)
    }
    #(Deref(t), ty, env) -> {
      //  env |- t : ty ref
      // --------------------
      //  env |- !t : ty
      let new_rule = #(t, Tref(ty), env)
      generate_equations(acc, new_rule, counter, weaks)
    }
    #(Assign(ref, t), ty, env) -> {
      //  env |- ref : new_type ref  env |- t : new_type
      // -------------------------------------------------
      //       env |- ref := t : Tunit
      // new_equation : ty = Tunit
      let new_type = alpha.new_var(counter) |> Tvar
      let new_equation = #(Tunit, ty)
      let new_rule_ref = #(ref, Tref(new_type), env)
      let new_rule_t = #(t, new_type, env)
      use #(new_acc, new_weaks) <- result.try(generate_equations(
        [new_equation, ..acc],
        new_rule_t,
        counter,
        weaks,
      ))
      generate_equations(new_acc, new_rule_ref, counter, new_weaks)
    }
    #(Chan, ty, _) -> {
      //       
      // ----------------------------------
      //       env |- chan : ty
      // new equation : ty = new_type chan     
      let new_type = alpha.new_var(counter) |> Tvar |> Tchan
      let new_equation = #(ty, new_type)
      Ok(#([new_equation, ..acc], weaks))
    }
    #(Send(chan, message), ty, env) -> {
      //      env |- channel : ty chan         env |- message : ty
      // ---------------------------------------------------------
      //       env |- Send(channel, message) : ty
      use #(new_weaks, new_ty) <- result.try(inference_auxiliary(
        message,
        env,
        counter,
        weaks,
      ))
      let new_equation = #(new_ty, ty)
      let new_rule = #(chan, Tchan(new_ty), env)
      generate_equations([new_equation, ..acc], new_rule, counter, new_weaks)
    }
    #(Recv(chan), ty, env) -> {
      //      env |- channel : ty chan
      // ------------------------------------
      //      env |- Recv(channel) : ty
      let new_rule = #(chan, Tchan(ty), env)
      generate_equations(acc, new_rule, counter, weaks)
    }
    #(Fork(expr1, expr2), ty, env) -> {
      use #(new_weaks, _) <- result.try(inference_auxiliary(
        expr1,
        env,
        counter,
        weaks,
      ))
      use #(new_weaks, _) <- result.try(inference_auxiliary(
        expr2,
        env,
        counter,
        new_weaks,
      ))
      let new_equation = #(ty, Tunit)
      Ok(#([new_equation, ..acc], new_weaks))
    }
    #(Str(_), ty, _) -> {
      let new_equation = #(Tstr, ty)
      Ok(#([new_equation, ..acc], weaks))
    }
    #(Print(t), ty, env) | #(Println(t), ty, env) -> {
      let new_equation = #(ty, Tunit)
      let new_rule = #(t, Tstr, env)
      generate_equations([new_equation, ..acc], new_rule, counter, weaks)
    }
    #(Call(obj, name), ty, env) -> {
      let new_rule = #(obj, Tobject([Tfield(name, ty)]), env)
      generate_equations(acc, new_rule, counter, weaks)
    }
    #(Object(fields), ty, env) -> {
      let tfields = generate_object_type(fields, counter)
      let new_type = ptype.new_object_type(tfields)
      let new_equation = #(ty, new_type)
      let new_rules = generate_object_rules([], fields, tfields, env)
      use #(new_acc, new_weaks), rule <- list.try_fold(new_rules, #(
        [new_equation, ..acc],
        weaks,
      ))
      generate_equations(new_acc, rule, counter, new_weaks)
    }
  }
}

const goal_string = "goal"

const goal_type = Tvar(goal_string)

/// Returns the final type of the goal and verify that the goal does not take multiple types
fn get_var_type(
  equations: Equations,
  goal: String,
  weaks: WeakTable,
) -> Result(#(WeakTable, Ptype), InferrenceError) {
  let goal_not_reached = Error(GoalNotReached("The term is not typeable"))
  use final_type, eq <- list.fold(equations, goal_not_reached)
  case eq {
    #(Tvar(v), ty) | #(ty, Tvar(v))
      if v == goal && final_type == goal_not_reached
    -> {
      Ok(#(bitable.replace_value(weaks, Tvar(goal), ty), ty))
    }
    #(Tvar(v), ty) | #(ty, Tvar(v)) if v == goal -> {
      case final_type {
        Ok(#(_, res_ty)) if res_ty == ty -> final_type
        Ok(#(_, res_ty)) ->
          Error(GoalContradiction("cannot unify goal type", #(res_ty, ty)))
        _ -> final_type
      }
    }
    _ -> final_type
  }
}

/// Returns the equations obtained from an equation between two object types (Ex : Tobject(types1) = Tobject(types2))
/// Precondition : types1 and types2 are sorted according to the names of their fields 
pub fn sub_types_equations(
  acc_equations: Equations,
  types1: List(Tfield),
  types2: List(Tfield),
) -> List(#(Ptype, Ptype)) {
  case types1, types2 {
    [], [] | [], _ | _, [] -> acc_equations
    [Tfield(name1, ty1), ..rest1], [Tfield(name2, ty2), ..rest2] -> {
      case string.compare(name1, name2) {
        order.Eq ->
          sub_types_equations([#(ty1, ty2), ..acc_equations], rest1, rest2)
        order.Lt -> sub_types_equations(acc_equations, rest1, types2)
        order.Gt -> sub_types_equations(acc_equations, types1, rest2)
      }
    }
  }
}

/// computes the result of the Tvar("goal") from the equations passed in argument
/// This functions is tail recursive
fn unification_auxiliary(
  equations: Equations,
  treated: Equations,
  goal: String,
  counter: CounterActor,
  weaks: WeakTable,
) -> Result(#(WeakTable, Ptype), InferrenceError) {
  case equations {
    [] -> get_var_type(treated, goal, weaks)
    [#(Polymorphic(t1), t2), ..rest] | [#(t2, Polymorphic(t1)), ..rest] -> {
      let new_ty1 = ptype.poly_rename(t1, counter)
      unification_auxiliary(
        [#(new_ty1, t2), ..rest],
        treated,
        goal,
        counter,
        weaks,
      )
    }
    [#(Weak(v), t), ..rest] | [#(t, Weak(v)), ..rest] -> {
      // TODO care potential bug after replacing rest by equations
      let new_equations =
        substitute_type_equations(weaks, list.append(rest, treated), v, t)
      let new_weaks = bitable.insert_key(weaks, v, t)
      unification_auxiliary(
        new_equations,
        [#(Tvar(v), t)],
        goal,
        counter,
        new_weaks,
      )
    }
    [#(Tvar(v), _) as g, ..rest] | [#(_, Tvar(v)) as g, ..rest] if v == goal -> {
      unification_auxiliary(rest, [g, ..treated], goal, counter, weaks)
    }
    [#(Tvar(v) as t1, t2), ..rest] | [#(t2, Tvar(v) as t1), ..rest] -> {
      case in_type(v, t2) {
        False -> {
          let new_equations =
            substitute_type_equations(weaks, list.append(rest, treated), v, t2)
          let new_weaks = bitable.replace_value(weaks, t1, t2)
          unification_auxiliary(new_equations, [], goal, counter, new_weaks)
        }
        True if t1 == t2 ->
          unification_auxiliary(rest, treated, goal, counter, weaks)
        True ->
          Error(
            UnificationError(
              "cannot unify variable type with type containing it",
              #(t1, t2),
            ),
          )
      }
    }
    [#(Tapp(t1, t2), Tapp(t3, t4)), ..rest] ->
      // We add the equations (t3, t1) instead of (t1, t3) because order matters when typing object
      // we have :
      //  t3 <= t1  t2 <= t4
      // --------------------
      // t1 -> t2 <= t3 -> t4 
      unification_auxiliary(
        [#(t3, t1), #(t2, t4), ..rest],
        treated,
        goal,
        counter,
        weaks,
      )
    [#(Tapp(_, _) as t1, t2), ..] | [#(t1, Tapp(_, _) as t2), ..] -> {
      Error(
        UnificationError("cannot unify app and non application type", #(t1, t2)),
      )
    }
    [#(Tinteger, Tinteger), ..rest]
    | [#(Tunit, Tunit), ..rest]
    | [#(Tstr, Tstr), ..rest] ->
      unification_auxiliary(rest, treated, goal, counter, weaks)
    [#(Tlist(ty1), Tlist(ty2)), ..rest] ->
      unification_auxiliary(
        [#(ty1, ty2), ..rest],
        treated,
        goal,
        counter,
        weaks,
      )
    [#(Tref(ty1), Tref(ty2)), ..rest] ->
      unification_auxiliary(
        [#(ty1, ty2), ..rest],
        treated,
        goal,
        counter,
        weaks,
      )
    [#(Tchan(ty1), Tchan(ty2)), ..rest] ->
      unification_auxiliary(
        [#(ty1, ty2), ..rest],
        treated,
        goal,
        counter,
        weaks,
      )
    [#(Tobject(types1) as t1, Tobject(types2) as t2), ..rest] -> {
      sub_types_equations(rest, types1, types2)
      |> unification_auxiliary([#(t1, t2), ..treated], goal, counter, weaks)
    }
    [#(Tlist(_) as t1, t2), ..] | [#(t1, Tlist(_) as t2), ..] ->
      Error(UnificationError("cannot unify list with non list type", #(t1, t2)))
    [#(Tref(_) as t1, t2), ..] | [#(t1, Tref(_) as t2), ..] ->
      Error(UnificationError("cannot unify ref with non ref type", #(t1, t2)))
    [#(Tchan(_) as t1, t2), ..] | [#(t1, Tchan(_) as t2), ..] ->
      Error(
        UnificationError("cannot unify chan type with non chan type", #(t1, t2)),
      )
    [#(Tobject(_) as t1, t2), ..] | [#(t1, Tobject(_) as t2), ..] ->
      Error(
        UnificationError("cannot unify object type with non object type", #(
          t1,
          t2,
        )),
      )
    [#(Tinteger as t1, t2), ..]
    | [#(t1, Tinteger as t2), ..]
    | [#(Tunit as t1, t2), ..]
    | [#(t1, Tunit as t2), ..] ->
      Error(
        UnificationError("cannot unify atomic type with different type", #(
          t1,
          t2,
        )),
      )
  }
}

/// auxiliary function of inference
fn inference_auxiliary(
  term: Pterm,
  env: TypingEnv,
  act: CounterActor,
  weaks: WeakTable,
) -> Result(#(WeakTable, Ptype), InferrenceError) {
  use #(equations, new_weaks) <- result.try(generate_equations(
    [],
    #(term, goal_type, env),
    act,
    weaks,
  ))
  unification_auxiliary(equations, [], goal_string, act, new_weaks)
}

/// infers the type of term t
pub fn inference(t: Pterm) -> Result(Ptype, InferrenceError) {
  let counter = alpha.new_counter()
  use #(_, res) <- result.map(inference_auxiliary(
    t,
    dict.new(),
    counter,
    bitable.new(),
  ))
  res
}
