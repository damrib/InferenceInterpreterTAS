import alpha
import bitable
import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import term.{
  type Pterm, Abs, Add, App, Assign, Chan, Cons, Deref, Empty, Fork, Head, Ife,
  Ifz, Integer, Let, Print, Println, Rec, Recv, Ref, Send, Str, Tail, Unit, Var,
}

pub type Ptype {
  Tvar(ty: String)
  Tapp(ty1: Ptype, ty2: Ptype)
  Tlist(ty: Ptype)
  Tinteger
  Tstr
  Tunit
  Tref(ty: Ptype)
  Tchan(ty: Ptype)
  Weak(ty: String)
  // only used during the unification
  Polymorphic(ty: Ptype)
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

pub type TypingEnv =
  dict.Dict(String, Ptype)

pub type WeakTable =
  bitable.Bitable(String, Ptype)

type Equation =
  #(Ptype, Ptype)

pub type Equations =
  List(Equation)

pub fn type_equality(ty1: Ptype, ty2: Ptype) -> Bool {
  case ty1, ty2 {
    Tvar(_), _ -> True
    _, Tvar(_) -> True
    Tapp(ta1, ta2), Tapp(ta3, ta4) ->
      type_equality(ta1, ta3) || type_equality(ta2, ta4)
    Tlist(tl1), Tlist(tl2) -> type_equality(tl1, tl2)
    _, _ -> ty1 == ty2
  }
}

/// this function is used to display prettier type names and to avoid confusing
/// ('_v2 -> '_v2) ref with '_v2 -> ('_v2 ref)
fn string_of_type_aux(ptype: Ptype) -> String {
  case ptype {
    Tapp(ty1, ty2) -> {
      let string_type1 = string_of_type_aux(ty1)
      let string_type2 = string_of_type_aux(ty2)
      string.concat(["(", string_type1, " -> ", string_type2, ")"])
    }
    _ -> string_of_type(ptype)
  }
}

pub fn string_of_type(ptype: Ptype) -> String {
  case ptype {
    Tvar(ty) -> ty
    Tapp(ty1, ty2) -> {
      let string_type1 = string_of_type_aux(ty1)
      let string_type2 = string_of_type_aux(ty2)
      string.concat([string_type1, " -> ", string_type2])
    }
    Tref(ty) -> string.append(string_of_type_aux(ty), " ref")
    Tlist(ty) -> string.append(string_of_type(ty), " list")
    Tchan(ty) -> string.append(string_of_type(ty), " chan")
    Tunit -> "unit"
    Tinteger -> "int"
    Tstr -> "str"
    Weak(ty) -> string.append("'_", ty)
    Polymorphic(ty) -> string.append("'", string_of_type(ty))
  }
}

pub fn pretty_rename(ty: Ptype) -> Ptype {
  let utf_code = fn(a) {
    let assert Ok(utf) = string.utf_codepoint(a)
    string.from_utf_codepoints([utf])
  }

  rename_type(ty, utf_code, 97).0
}

pub fn poly_rename(ty: Ptype, counter: CounterActor) -> Ptype {
  let count = alpha.get_count(counter)
  let #(new_type, add_counter) =
    rename_type(
      ty,
      fn(i) { string.append("v", int.to_string(i + count)) },
      count,
    )
  alpha.add_count(counter, add_counter)
  new_type
}

pub fn rename_type(
  ty: Ptype,
  code: fn(Int) -> String,
  start: Int,
) -> #(Ptype, Int) {
  let type_names = list.unique(enumerate_type_name(ty, []))
  let length = list.length(type_names)

  let new_names = list.map(list.range(start, start + length), code)
  let d = dict.from_list(list.zip(type_names, new_names))
  #(change_var_name(ty, d), start + length)
}

fn get_type(env: TypingEnv, var: String) -> Result(Ptype, Nil) {
  case dict.get(env, var) {
    Ok(ty) -> Ok(ty)
    Error(Nil) -> panic as "error when getting the type"
  }
}

fn propagate_weak(
  env: TypingEnv,
  weaks: WeakTable,
  ty: Ptype,
) -> #(TypingEnv, WeakTable, Ptype) {
  case ty {
    Tvar(name) -> {
      // TODO : delete weaks from env
      let new_weaks = bitable.insert_key(weaks, name, Weak(name))
      #(env, new_weaks, Weak(name))
    }
    Tlist(t1) -> {
      let #(new_env, new_weaks, new_ty) = propagate_weak(env, weaks, t1)
      #(new_env, new_weaks, Tlist(new_ty))
    }
    Tref(t1) -> {
      let #(new_env, new_weaks, new_ty) = propagate_weak(env, weaks, t1)
      #(new_env, new_weaks, Tref(new_ty))
    }
    Tchan(t1) -> {
      let #(new_env, new_weaks, new_ty) = propagate_weak(env, weaks, t1)
      #(new_env, new_weaks, Tchan(new_ty))
    }
    Tapp(t1, t2) -> {
      let #(new_env, new_weaks, new_t1) = propagate_weak(env, weaks, t1)
      let #(new_env, new_weaks, new_t2) = propagate_weak(new_env, new_weaks, t2)
      #(new_env, new_weaks, Tapp(new_t1, new_t2))
    }
    Weak(_) -> #(env, weaks, ty)
    Polymorphic(_) -> panic as "should not happen"
    _ -> #(env, weaks, ty)
  }
}

fn add_atomic(env: TypingEnv, name: String, ty: Ptype) -> TypingEnv {
  dict.insert(env, name, ty)
}

fn add_polymorphic(
  env: TypingEnv,
  weaks: WeakTable,
  name: String,
  term: Pterm,
  var: Ptype,
) -> #(TypingEnv, WeakTable) {
  case term.is_expansive(term) {
    True -> {
      let #(new_env, new_weaks, new_ty) = propagate_weak(env, weaks, var)
      #(dict.insert(new_env, name, new_ty), new_weaks)
    }
    False -> #(dict.insert(env, name, Polymorphic(var)), weaks)
  }
}

fn in_type(var: String, ty: Ptype) -> Bool {
  case ty {
    Tvar(v) if v == var -> True
    Tapp(ty1, ty2) -> in_type(var, ty1) || in_type(var, ty2)
    Tlist(ty) | Tref(ty) -> in_type(var, ty)
    _ -> False
  }
}

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
        // TODO
        Error(_) -> ty
      }
    }
    _ -> ty
  }
}

fn substitute_type_equations(
  weaks: WeakTable,
  equations: Equations,
  var: String,
  new_type: Ptype,
) -> Equations {
  fn(eq: #(Ptype, Ptype)) -> Equation {
    let new_ty1 = substitute_type(weaks, eq.0, var, new_type)
    let new_ty2 = substitute_type(weaks, eq.1, var, new_type)
    #(new_ty1, new_ty2)
  }
  |> list.map(equations, _)
}

fn generate_list_equations(
  acc: List(#(Pterm, Ptype, TypingEnv)),
  list: Pterm,
  elem_type: Ptype,
  env: TypingEnv,
) -> List(#(Pterm, Ptype, TypingEnv)) {
  case list {
    Cons(elem, rest) ->
      generate_list_equations(
        [#(elem, elem_type, env), ..acc],
        rest,
        elem_type,
        env,
      )
    Deref(Var(_)) -> [#(list, Tlist(elem_type), env), ..acc]
    Empty -> acc
    _ ->
      panic as string.append(
          "generate_list_equations should only be used on Cons and Empty : ",
          term.string_of_term(list, 0),
        )
  }
}

fn weak_type_or(weaks: WeakTable, ty: Ptype) -> Ptype {
  case ty {
    Weak(name) -> {
      case bitable.get_value(weaks, name) {
        Ok(res) -> res
        Error(_) -> ty
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
    _ -> ty
  }
}

/// auxiliary function of generate_equations used to do tail recursive
fn generate_equations_auxiliary(
  acc: Equations,
  remaining: List(Rule),
  counter: CounterActor,
  weaks: WeakTable,
) -> Result(#(Equations, WeakTable), InferrenceError) {
  case remaining {
    [] -> Ok(#(acc, weaks))
    [#(Var(v), ty, env), ..rest] -> {
      let assert Ok(var_type) = get_type(env, v)
        as "a variable has not been typed"
      let new_equation = #(weak_type_or(weaks, var_type), ty)

      generate_equations_auxiliary([new_equation, ..acc], rest, counter, weaks)
    }
    [#(App(term1, term2), ty, env), ..rest] -> {
      let new_type = alpha.new_var(counter)
      let new_eq1 = #(term1, Tapp(Tvar(new_type), ty), env)
      let new_eq2 = #(term2, Tvar(new_type), env)
      generate_equations_auxiliary(
        acc,
        [new_eq2, new_eq1, ..rest],
        counter,
        weaks,
      )
    }
    [#(Abs(x, term), ty, env), ..rest] -> {
      let new_type1 = alpha.new_var(counter)
      let new_type2 = alpha.new_var(counter)
      let new_equation = #(ty, Tapp(Tvar(new_type1), Tvar(new_type2)))
      let new_env = add_atomic(env, x, Tvar(new_type1))
      generate_equations_auxiliary(
        [new_equation, ..acc],
        [#(term, Tvar(new_type2), new_env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Integer(_), ty, _), ..rest] -> {
      generate_equations_auxiliary(
        [#(Tinteger, ty), ..acc],
        rest,
        counter,
        weaks,
      )
    }
    [#(Add(t1, t2), ty, env), ..rest] -> {
      let new_eq1 = #(t1, Tinteger, env)
      let new_eq2 = #(t2, Tinteger, env)
      generate_equations_auxiliary(
        [#(Tinteger, ty), ..acc],
        [new_eq2, new_eq1, ..rest],
        counter,
        weaks,
      )
    }
    [#(Cons(_, _) as l, ty, env), ..rest] -> {
      let type_name = alpha.new_var(counter)
      let new_type = Tvar(type_name)
      let remaining_eqs = generate_list_equations(rest, l, new_type, env)
      generate_equations_auxiliary(
        [#(ty, Tlist(new_type)), ..acc],
        remaining_eqs,
        counter,
        weaks,
      )
    }
    [#(Head(l), ty, env), ..rest] -> {
      //   env |- l : Tlist(t) 
      // ---------------------- 
      //   env |- Head(l) : t 
      generate_equations_auxiliary(
        acc,
        [#(l, Tlist(ty), env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Tail(l), ty, env), ..rest] -> {
      //    env |- l : t 
      // -------------------- 
      //  env |- Tail(l) : t
      generate_equations_auxiliary(acc, [#(l, ty, env), ..rest], counter, weaks)
    }
    [#(Empty, ty, _), ..rest] -> {
      let type_name = alpha.new_var(counter)
      let new_type = Tvar(type_name)
      generate_equations_auxiliary(
        [#(Tlist(new_type), ty), ..acc],
        rest,
        counter,
        weaks,
      )
    }
    [#(Ife(c, t, e), ty, env), ..rest] -> {
      // env |- c : t0 list env |- t : t1 env |- e : t1
      // -----------------------------------------------
      //               Ife(c, t, e) : t1
      let type_name = alpha.new_var(counter)
      let new_type = Tlist(Tvar(type_name))
      generate_equations_auxiliary(
        acc,
        [#(c, new_type, env), #(t, ty, env), #(e, ty, env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Ifz(c, t, e), ty, env), ..rest] -> {
      // env |- c : int env |- t : t1 env |- e : t1
      // -----------------------------------------------
      //               Ife(c, t, e) : t1
      generate_equations_auxiliary(
        acc,
        [#(c, Tinteger, env), #(t, ty, env), #(e, ty, env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Let(var, bind, e), ty, env), ..rest] -> {
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
      generate_equations_auxiliary(
        acc,
        [#(e, ty, new_env), ..rest],
        counter,
        new_weaks,
      )
    }
    [#(Rec(name, fun), ty, env), ..rest] -> {
      let new_env = add_atomic(env, name, ty)
      generate_equations_auxiliary(
        acc,
        [#(fun, ty, new_env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Unit, ty, _), ..rest] ->
      generate_equations_auxiliary([#(Tunit, ty), ..acc], rest, counter, weaks)
    [#(Ref(t), ty, env), ..rest] -> {
      //   env |- t : new_type
      // --------------------------------
      //   env |- ref t : new_type ref
      // new_equation : ty = new_type ref
      let new_var = alpha.new_var(counter)
      let new_type = Tvar(new_var)
      generate_equations_auxiliary(
        [#(Tref(new_type), ty), ..acc],
        [#(t, new_type, env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Deref(t), ty, env), ..rest] -> {
      //  env |- t : ty ref
      // --------------------
      //  env |- !t : ty
      generate_equations_auxiliary(
        acc,
        [#(t, Tref(ty), env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Assign(ref, t), ty, env), ..rest] -> {
      //  env |- ref : new_type ref  env |- t : new_type
      // -------------------------------------------------
      //       env |- ref := t : Tunit
      // new_equation : ty = Tunit
      let new_var = alpha.new_var(counter)
      let new_type = Tvar(new_var)
      generate_equations_auxiliary(
        [#(Tunit, ty), ..acc],
        [#(ref, Tref(new_type), env), #(t, new_type, env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Chan, ty, _), ..rest] -> {
      //       
      // ----------------------------------
      //       env |- chan : ty
      // new equation : ty = new_type chan
      let new_var = alpha.new_var(counter)
      let new_type = Tchan(Tvar(new_var))
      generate_equations_auxiliary(
        [#(ty, new_type), ..acc],
        rest,
        counter,
        weaks,
      )
    }
    [#(Send(chan, message), ty, env), ..rest] -> {
      //      env |- channel : ty chan         env |- message : ty
      // ---------------------------------------------------------
      //       env |- Send(channel, message) : ty
      // TODO: ajout weak pour chan? Care bug env diff left right
      use #(new_weaks, new_ty) <- result.try(inference_auxiliary(
        message,
        env,
        counter,
        weaks,
      ))
      generate_equations_auxiliary(
        [#(new_ty, ty), ..acc],
        [#(chan, Tchan(new_ty), env), ..rest],
        counter,
        new_weaks,
      )
    }
    [#(Recv(chan), ty, env), ..rest] -> {
      //      env |- channel : ty chan
      // ------------------------------------
      //      env |- Recv(channel) : ty
      generate_equations_auxiliary(
        acc,
        [#(chan, Tchan(ty), env), ..rest],
        counter,
        weaks,
      )
    }
    [#(Fork(expr1, expr2), ty, env), ..rest] -> {
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
      generate_equations_auxiliary(
        [#(ty, Tunit), ..acc],
        rest,
        counter,
        new_weaks,
      )
    }
    [#(Str(_), ty, _), ..rest] ->
      generate_equations_auxiliary([#(ty, Tstr), ..acc], rest, counter, weaks)
    [#(Print(t), ty, env), ..rest] | [#(Println(t), ty, env), ..rest] -> {
      generate_equations_auxiliary(
        [#(ty, Tunit), ..acc],
        [#(t, Tstr, env), ..rest],
        counter,
        weaks,
      )
    }
  }
}

const goal_string = "goal"

const goal_type = Tvar(goal_string)

fn generate_equations(
  term: Pterm,
  counter: CounterActor,
) -> Result(Equations, InferrenceError) {
  generate_equations_auxiliary(
    [],
    [#(term, goal_type, dict.new())],
    counter,
    bitable.new(),
  )
  |> result.map(fn(res) { res.0 })
}

// TODO potentiel bug
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

pub fn is_non_trivial(equation: Equation) -> Bool {
  case equation {
    #(Tinteger, Tinteger) -> False
    _ -> True
  }
}

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
      let new_ty1 = poly_rename(t1, counter)
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
    [#(Tvar(v), _) as g, ..rest] | [#(_, Tvar(v)) as g, ..rest] if v == goal ->
      unification_auxiliary(rest, [g, ..treated], goal, counter, weaks)
    [#(Tvar(v) as t1, t2), ..rest] | [#(t2, Tvar(v) as t1), ..rest] -> {
      case in_type(v, t2) {
        False -> {
          let new_equations =
            substitute_type_equations(weaks, list.append(rest, treated), v, t2)
          // TODO: CARE BUG Weak(t1)?
          let new_weaks = bitable.replace_value(weaks, t1, t2)
          unification_auxiliary(new_equations, [], goal, counter, new_weaks)
        }

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
      unification_auxiliary(
        [#(t1, t3), #(t2, t4), ..rest],
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
    [#(Tlist(_) as t1, t2), ..] | [#(t1, Tlist(_) as t2), ..] ->
      Error(UnificationError("cannot unify list with non list type", #(t1, t2)))
    [#(Tref(_) as t1, t2), ..] | [#(t1, Tref(_) as t2), ..] ->
      Error(UnificationError("cannot unify ref with non ref type", #(t1, t2)))
    [#(Tchan(_) as t1, t2), ..] | [#(t1, Tchan(_) as t2), ..] ->
      Error(
        UnificationError("cannot unify chan type with non chan type", #(t1, t2)),
      )
    _ -> panic as "not implemented"
  }
}

fn unification(
  equations: Equations,
  counter: CounterActor,
) -> Result(Ptype, InferrenceError) {
  let res =
    unification_auxiliary(equations, [], goal_string, counter, bitable.new())
  result.map(res, fn(ok) { ok.1 })
}

type CounterActor =
  alpha.AlphaActor

fn inference_auxiliary(
  term: Pterm,
  env: TypingEnv,
  act: CounterActor,
  weaks: WeakTable,
) -> Result(#(WeakTable, Ptype), InferrenceError) {
  use #(equations, new_weaks) <- result.try(generate_equations_auxiliary(
    [],
    [#(term, goal_type, env)],
    act,
    weaks,
  ))
  unification_auxiliary(equations, [], goal_string, act, new_weaks)
}

pub fn inference(t: Pterm) -> Result(Ptype, InferrenceError) {
  let counter = alpha.new_counter()
  use equations <- result.try(generate_equations(t, counter))
  unification(equations, counter)
}

fn enumerate_type_name(ty: Ptype, acc: List(String)) -> List(String) {
  case ty {
    Tvar(v) -> [v, ..acc]
    Tapp(ty1, ty2) -> {
      let acc = enumerate_type_name(ty2, acc)
      enumerate_type_name(ty1, acc)
    }
    Tref(ty) -> enumerate_type_name(ty, acc)
    Tlist(ty) -> enumerate_type_name(ty, acc)
    Tchan(ty) -> enumerate_type_name(ty, acc)
    Tunit -> acc
    Tinteger -> acc
    Tstr -> acc
    Weak(ty) -> [ty, ..acc]
    Polymorphic(ty) -> enumerate_type_name(ty, acc)
  }
}

fn change_var_name(ty: Ptype, d: dict.Dict(String, String)) -> Ptype {
  case ty {
    Tvar(v) -> {
      let assert Ok(new_name) = dict.get(d, v)
      Tvar(new_name)
    }
    Tapp(ty1, ty2) -> {
      let new_ty1 = change_var_name(ty1, d)
      let new_ty2 = change_var_name(ty2, d)
      Tapp(new_ty1, new_ty2)
    }
    Tchan(ty) -> {
      let new_ty = change_var_name(ty, d)
      Tchan(new_ty)
    }
    Tref(ty) -> {
      let new_ty = change_var_name(ty, d)
      Tref(new_ty)
    }
    Tlist(ty) -> {
      let new_ty = change_var_name(ty, d)
      Tlist(new_ty)
    }
    Weak(name) -> {
      let assert Ok(new_name) = dict.get(d, name)
      Weak(new_name)
    }
    t -> t
  }
}
