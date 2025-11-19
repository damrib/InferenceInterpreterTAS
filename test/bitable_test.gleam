import bitable
import gleam/list
import gleam/result
import gleeunit

pub fn main() {
  gleeunit.main()
}

fn belongs_to(l: List(a), key: a) -> Result(Bool, Nil) {
  Ok(list.any(l, fn(e) { e == key }))
}

// PostCondition to verify
// Input: table
// Output: new_table
// After insert key, val
// size(table) + 1 = size(new_table) if key not in bitable
// size(table) = size(new_table) otherwise
// has_value(new_table, val) is true
// has_key(new_table, key) is true
// key belongs to get_keys(new_table, val)
// Ok(val) equals to get_value(new_table, key)
pub fn insert_key_test() {
  let table = bitable.new()

  let key = 1
  let val = "0"
  let new_table = bitable.insert_key(table, 1, val)

  assert !bitable.has_key(table, key)
  assert bitable.size(table) + 1 == bitable.size(new_table)
  assert bitable.has_value(new_table, val)
  assert bitable.has_key(new_table, key)
  assert Ok(True)
    == result.try(bitable.get_keys(new_table, val), belongs_to(_, key))
  assert Ok(val) == bitable.get_value(new_table, key)

  let val = "1"
  let new_table1 = bitable.insert_key(new_table, 1, val)

  assert bitable.has_key(new_table1, key)
  assert bitable.size(new_table) == bitable.size(new_table1)
  assert bitable.has_value(new_table1, val)
  assert bitable.has_key(new_table1, key)
  assert Ok(True)
    == result.try(bitable.get_keys(new_table1, val), belongs_to(_, key))
  assert Ok(val) == bitable.get_value(new_table1, key)
}
