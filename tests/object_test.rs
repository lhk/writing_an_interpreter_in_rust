use writing_an_interpreter_in_rust::ast::HashKey;

#[test]
fn test_string_hash_key() {
    let h1 = HashKey::String("Hello World".to_string());
    let h2 = HashKey::String("Hello World".to_string());
    let d1 = HashKey::String("My name is johnny".to_string());
    let d2 = HashKey::String("My name is johnny".to_string());

    assert_eq!(h1, h2, "strings with same content have different hash keys");
    assert_eq!(d1, d2, "strings with same content have different hash keys");
    assert_ne!(h1, d1, "strings with different content have same hash keys");
}

#[test]
fn test_boolean_hash_key() {
    let t1 = HashKey::Bool(true);
    let t2 = HashKey::Bool(true);
    let f1 = HashKey::Bool(false);
    let f2 = HashKey::Bool(false);

    assert_eq!(t1, t2, "trues do not have same hash key");
    assert_eq!(f1, f2, "falses do not have same hash key");
    assert_ne!(t1, f1, "true has same hash key as false");
}

#[test]
fn test_integer_hash_key() {
    let o1 = HashKey::Int(1);
    let o2 = HashKey::Int(1);
    let t1 = HashKey::Int(2);
    let t2 = HashKey::Int(2);

    assert_eq!(
        o1, o2,
        "integers with same content have different hash keys"
    );
    assert_eq!(
        t1, t2,
        "integers with same content have different hash keys"
    );
    assert_ne!(
        o1, t1,
        "integers with different content have same hash keys"
    );
}
