import gleam/dict.{type Dict}
import gleam/list
import gleam/option

pub opaque type Bitable(key, value) {
  Bitable(key_map: Dict(key, value), value_table: Dict(value, List(key)))
}

pub fn new() -> Bitable(k, v) {
  Bitable(dict.new(), dict.new())
}

fn key_duplicates(value_table: Dict(v, List(k)), key: k) -> Bool {
  dict.fold(value_table, False, fn(b, _, keys) {
    b || list.any(keys, fn(e) { e == key })
  })
}

fn add_in_list(
  value_table: Dict(v, List(k)),
  value: v,
  key: k,
) -> Dict(v, List(k)) {
  fn(list) {
    option.unwrap(list, [])
    |> list.prepend(key)
  }
  |> dict.upsert(value_table, value, _)
}

fn list_filter_key(
  value_table: Dict(v, List(k)),
  value: v,
  key: k,
) -> Dict(v, List(k)) {
  let assert Ok(keys) = dict.get(value_table, value)

  let res = case list.filter(keys, fn(e) { e != key }) {
    [] -> dict.delete(value_table, value)
    l -> dict.insert(value_table, value, l)
  }

  assert !key_duplicates(res, key)

  res
}

pub fn insert_key(table: Bitable(k, v), key: k, value: v) -> Bitable(k, v) {
  case dict.get(table.key_map, key) {
    Ok(former) if former != value -> {
      let key_map = dict.insert(table.key_map, key, value)

      let value_table = list_filter_key(table.value_table, former, key)

      add_in_list(value_table, value, key)
      |> Bitable(key_map, _)
    }
    Error(_) -> {
      let key_map = dict.insert(table.key_map, key, value)

      add_in_list(table.value_table, value, key)
      |> Bitable(key_map, _)
    }
    _ -> table
  }
}

pub fn get_value(table: Bitable(k, v), key: k) -> Result(v, Nil) {
  dict.get(table.key_map, key)
}

pub fn get_keys(table: Bitable(k, v), value: v) -> Result(List(k), Nil) {
  dict.get(table.value_table, value)
}

pub fn from_list(l: List(#(k, v))) -> Bitable(k, v) {
  use table, #(key, value) <- list.fold(l, new())
  insert_key(table, key, value)
}

pub fn has_key(table: Bitable(k, v), key: k) -> Bool {
  dict.has_key(table.key_map, key)
}

pub fn has_value(table: Bitable(k, v), value: v) -> Bool {
  dict.has_key(table.value_table, value)
}

pub fn size(table: Bitable(k, v)) -> Int {
  dict.size(table.key_map)
}

pub fn replace_value(table: Bitable(k, v), former: v, fresh: v) -> Bitable(k, v) {
  let keys = get_keys(table, former)
  case keys {
    Ok(key_list) -> {
      let key_map = {
        use table, key <- list.fold(key_list, table.key_map)
        dict.insert(table, key, fresh)
      }

      let value_table = {
        dict.delete(table.value_table, former)
        |> dict.insert(fresh, key_list)
      }

      Bitable(key_map, value_table)
    }
    Error(_) -> table
  }
}

pub fn merge(table1: Bitable(k, v), table2: Bitable(k, v)) -> Bitable(k, v) {
  let keys = table2.key_map
  use table, key, value <- dict.fold(keys, table1)
  insert_key(table, key, value)
}

pub fn delete_key(table: Bitable(k, v), key: k) -> Bitable(k, v) {
  let value = dict.get(table.key_map, key)
  case value {
    Ok(val) -> {
      let key_map = dict.delete(table.key_map, key)

      let assert Ok(keys) = get_keys(table, val)
      let keys = list.filter(keys, fn(former) { former == key })
      let value_table = case list.is_empty(keys) {
        True -> dict.delete(table.value_table, val)
        False -> dict.insert(table.value_table, val, keys)
      }

      Bitable(key_map, value_table)
    }
    Error(_) -> table
  }
}

pub fn delete_value(table: Bitable(k, v), value: v) -> Bitable(k, v) {
  let keys = get_keys(table, value)
  case keys {
    Ok(keys_list) -> {
      let key_map = {
        use table, key <- list.fold(keys_list, table.key_map)
        dict.delete(table, key)
      }

      let value_table = dict.delete(table.value_table, value)

      Bitable(key_map, value_table)
    }
    Error(_) -> table
  }
}

pub fn complementary(
  table1: Bitable(k, v),
  table2: Bitable(k, v),
) -> Bitable(k, v) {
  let key_map = {
    use map, k, v <- dict.fold(table1.key_map, dict.new())
    case dict.has_key(table2.key_map, k) {
      False -> dict.insert(map, k, v)
      True -> map
    }
  }

  let value_table = {
    use table, k, v <- dict.fold(key_map, dict.new())
    dict.upsert(table, v, fn(l) { [k, ..option.lazy_unwrap(l, list.new)] })
  }

  Bitable(key_map, value_table)
}
