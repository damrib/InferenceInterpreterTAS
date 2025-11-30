import alpha
import gleam/dict
import gleam/int
import gleam/list
import gleam/order
import gleam/string

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
  Tobject(range: List(Tfield))
  // only used during the unification
  Polymorphic(ty: Ptype)
}

pub type Tfield {
  Tfield(name: String, ty: Ptype)
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

/// this functions an object representing the field of an object and return its string representation
fn string_of_method_type(field: Tfield) -> String {
  string.concat([" \"", field.name, "\" : ", string_of_type(field.ty)])
}

/// This function takes a type and returns its string representation
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
    Tobject(range: list_ty) -> {
      let object_string =
        list.map(list_ty, string_of_method_type) |> string.concat
      string.concat(["{", object_string, "}"])
    }
  }
}

/// This function takes a type ty and renames the inner types of ty using the code function
fn rename_type(ty: Ptype, code: fn(Int) -> String, start: Int) -> #(Ptype, Int) {
  let type_names = list.unique(enumerate_type_name(ty, []))
  let length = list.length(type_names)

  let new_names = list.map(list.range(start, start + length), code)
  let d = dict.from_list(list.zip(type_names, new_names))
  #(change_var_name(ty, d), start + length)
}

/// This function takes a type ty and renames the inner types of ty
/// For instance : v1 -> v2 become a -> b
pub fn pretty_rename(ty: Ptype) -> Ptype {
  let utf_code = fn(a) {
    let assert Ok(utf) = string.utf_codepoint(a)
    string.from_utf_codepoints([utf])
  }

  rename_type(ty, utf_code, 97).0
}

/// This function renames the inner types of a type (function used when renaming polymorphic types)
pub fn poly_rename(ty: Ptype, counter: alpha.AlphaActor) -> Ptype {
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
    Tobject(ty_list) -> {
      use acc, field <- list.fold(ty_list, acc)
      enumerate_type_name(field.ty, acc)
    }
  }
}

/// changes the name of the inner types according to the new names they are associated with in d
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
    Tobject(list_ty) -> {
      {
        use field <- list.map(list_ty)
        Tfield(field.name, change_var_name(field.ty, d))
      }
      |> list.sort(compare_field_names)
      |> Tobject
    }
    t -> t
  }
}

/// This function comapres two fields according to their names
pub fn compare_field_names(field1: Tfield, field2: Tfield) -> order.Order {
  string.compare(field1.name, field2.name)
}

/// this functions returns a type object where the fields have been sorted
pub fn new_object_type(fields: List(Tfield)) -> Ptype {
  list.sort(fields, compare_field_names) |> Tobject
}
