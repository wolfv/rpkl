//! Snapshot tests for PKL evaluation

use rpkl_parser::parse_module;
use rpkl_runtime::Evaluator;
use rpkl_stdlib::stdlib_registry;

fn eval_pkl(source: &str) -> String {
    let module = parse_module(source).expect("Failed to parse");
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    let result = evaluator.eval_module(&module).expect("Failed to evaluate");
    evaluator.force_value(&result).expect("Failed to force");
    serde_json::to_string_pretty(&result).expect("Failed to serialize")
}

fn eval_pkl_result(source: &str) -> String {
    let module = match parse_module(source) {
        Ok(m) => m,
        Err(e) => return format!("Parse error: {}", e),
    };
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    match evaluator.eval_module(&module) {
        Ok(result) => {
            if let Err(e) = evaluator.force_value(&result) {
                return format!("Force error: {}", e);
            }
            serde_json::to_string_pretty(&result).expect("Failed to serialize")
        }
        Err(e) => format!("Eval error: {}", e),
    }
}

// =============================================================================
// Basic Types
// =============================================================================

#[test]
fn test_literals() {
    let source = r#"
        null_val = null
        bool_true = true
        bool_false = false
        int_val = 42
        float_val = 3.14
        string_val = "hello"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_interpolation() {
    let source = r#"
        name = "world"
        greeting = "Hello, \(name)!"
        nested = "Result: \(1 + 2)"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Arithmetic
// =============================================================================

#[test]
fn test_arithmetic() {
    let source = r#"
        add = 1 + 2
        sub = 10 - 3
        mul = 4 * 5
        div = 20 / 4
        int_div = 7 ~/ 2
        mod = 17 % 5
        pow = 2 ** 10
        precedence = 1 + 2 * 3
        parens = (1 + 2) * 3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_float_arithmetic() {
    let source = r#"
        add = 1.5 + 2.5
        mul = 2.5 * 4.0
        div = 10.0 / 4.0
        mixed = 1 + 2.5
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Comparisons and Logic
// =============================================================================

#[test]
fn test_comparisons() {
    let source = r#"
        eq = 1 == 1
        ne = 1 != 2
        lt = 1 < 2
        le = 2 <= 2
        gt = 3 > 2
        ge = 3 >= 3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_logical_operators() {
    let source = r#"
        and_tt = true && true
        and_tf = true && false
        or_tf = true || false
        or_ff = false || false
        not_t = !true
        not_f = !false
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_null_coalescing() {
    let source = r#"
        with_value = "value" ?? "default"
        with_null = null ?? "default"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Control Flow
// =============================================================================

#[test]
fn test_conditionals() {
    let source = r#"
        if_true = if (true) "yes" else "no"
        if_false = if (false) "yes" else "no"
        if_expr = if (1 < 2) "less" else "greater"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_let_bindings() {
    let source = r#"
        result = let (x = 10) let (y = 20) x + y
        nested = let (a = 1) let (b = a + 1) let (c = b + 1) c
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Objects
// =============================================================================

// TODO: Parser doesn't yet support nested object syntax without leading property names
// #[test]
// fn test_nested_objects() {
//     let source = r#"
//         outer = new {
//             inner = new {
//                 value = 42
//             }
//             name = "test"
//         }
//     "#;
//     insta::assert_snapshot!(eval_pkl(source));
// }

#[test]
fn test_simple_object() {
    let source = r#"
        name = "test"
        value = 42
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_listing() {
    let source = r#"
        items = new Listing {
            1
            2
            3
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// Note: Multiple Mapping entries require semicolons between them
#[test]
fn test_mapping_multiple_entries() {
    let source = r#"
        config = new Mapping {
            ["key1"] = "value1"; ["key2"] = "value2"; ["key3"] = 42
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_single_entry_mapping() {
    let source = r#"
        config = new Mapping { ["key1"] = "value1" }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Duration and DataSize
// =============================================================================

#[test]
fn test_duration() {
    let source = r#"
        ns = 100.ns
        ms = 500.ms
        s = 30.s
        min = 5.min
        h = 2.h
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_data_size() {
    let source = r#"
        bytes = 1024.b
        kb = 10.kb
        mb = 256.mb
        gb = 1.gb
        mib = 512.mib
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Lambda and Higher-Order Functions
// =============================================================================

#[test]
fn test_lambda() {
    let source = r#"
        double = (x) -> x * 2
        result = double.apply(21)
    "#;
    // Note: lambdas serialize as "<function>" in JSON
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_list_map() {
    let source = r#"
        numbers = new Listing { 1; 2; 3 }
        doubled = numbers.toList().map((x) -> x * 2)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_list_filter() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
            4
            5
        }
        evens = numbers.toList().filter((x) -> x % 2 == 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_fold() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
            4
            5
        }
        sum = numbers.toList().fold(0, (acc, x) -> acc + x)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_any_every() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
            4
            5
        }
        hasEven = numbers.toList().any((x) -> x % 2 == 0)
        allPositive = numbers.toList().every((x) -> x > 0)
        allEven = numbers.toList().every((x) -> x % 2 == 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String Methods
// =============================================================================

#[test]
fn test_string_methods() {
    let source = r#"
        str = "Hello, World!"
        len = str.length
        upper = str.toUpperCase()
        lower = str.toLowerCase()
        contains = str.contains("World")
        starts = str.startsWith("Hello")
        ends = str.endsWith("!")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_manipulation() {
    let source = r#"
        original = "  hello world  "
        trimmed = original.trim()
        replaced = "hello".replaceAll("l", "L")
        split_result = "a,b,c".split(",")
        chars = "abc".chars()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int/Float Methods
// =============================================================================

#[test]
fn test_int_methods() {
    // Note: abs, sign, isEven, isOdd are properties, not methods
    let source = r#"
        absVal = (-42).abs
        signVal = (-42).sign
        evenCheck = 4.isEven
        oddCheck = 5.isOdd
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_float_methods() {
    let source = r#"
        f = 3.7
        ceil_val = f.ceil()
        floor_val = f.floor()
        round_val = f.round()
        sqrt_val = 16.0.sqrt()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List Methods
// =============================================================================

#[test]
fn test_list_basic_methods() {
    let source = r#"
        items = new Listing {
            "a"
            "b"
            "c"
        }
        list = items.toList()
        len = list.length
        first_item = list.first
        last_item = list.last
        reversed = list.reverse()
        joined = list.join(", ")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_take_drop() {
    let source = r#"
        items = new Listing {
            1
            2
            3
            4
            5
        }
        list = items.toList()
        taken = list.take(3)
        dropped = list.drop(2)
        take_last = list.takeLast(2)
        drop_last = list.dropLast(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_contains() {
    let source = r#"
        items = new Listing {
            1
            2
            3
        }
        list = items.toList()
        has_2 = list.contains(2)
        has_5 = list.contains(5)
        idx = list.indexOf(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Complex Examples
// =============================================================================

#[test]
fn test_config_example() {
    let source = r#"
        name = "My Application"
        version = "1.0.0"
        port = 8080
        debug = true

        database {
            host = "localhost"
            port = 5432
            name = "myapp"
        }

        servers = new Listing {
            "server1.example.com"
            "server2.example.com"
        }

        settings {
            maxConnections = 100
            timeout = 30.s
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_computed_properties() {
    let source = r#"
        width = 10
        height = 20
        area = width * height
        perimeter = 2 * (width + height)
        description = "Rectangle: \(width)x\(height), area=\(area)"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// For/When Generators
// =============================================================================

#[test]
fn test_for_generator_basic() {
    let source = r#"
        numbers = new Listing {
            1
            2
            3
        }
        doubled = new Listing {
            for (n in numbers.toList()) {
                n * 2
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_for_generator_with_index() {
    let source = r#"
        items = new Listing {
            "a"
            "b"
            "c"
        }
        indexed = new Mapping {
            for (i, item in items.toList()) {
                [i] = item
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_when_generator_true() {
    let source = r#"
        debug = true
        config = new {
            name = "app"
            when (debug) {
                logLevel = "debug"
                verbose = true
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_when_generator_false() {
    let source = r#"
        debug = false
        config = new {
            name = "app"
            when (debug) {
                logLevel = "debug"
            } else {
                logLevel = "info"
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_nested_generators() {
    let source = r#"
        rows = new Listing { 1; 2 }
        cols = new Listing { 10; 20 }
        matrix = new Listing {
            for (r in rows.toList()) {
                for (c in cols.toList()) {
                    r + c
                }
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Classes
// =============================================================================

#[test]
fn test_class_basic() {
    let source = r#"
        class Person {
            name: String
            age: Int = 0
        }

        person1 = new Person {
            name = "Alice"
            age = 30
        }

        person2 = new Person {
            name = "Bob"
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_class_inheritance() {
    let source = r#"
        class Animal {
            name: String
            sound: String = "..."
        }

        class Dog extends Animal {
            sound = "woof"
            breed: String = "mixed"
        }

        animal = new Animal {
            name = "Generic"
        }

        dog = new Dog {
            name = "Rex"
            breed = "German Shepherd"
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_class_methods() {
    let source = r#"
        class Rectangle {
            width: Int
            height: Int

            function area() = width * height
            function perimeter() = 2 * (width + height)
        }

        rect = new Rectangle {
            width = 10
            height = 5
        }

        rectArea = rect.area()
        rectPerimeter = rect.perimeter()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_class_override_property() {
    let source = r#"
        class Config {
            host: String = "localhost"
            port: Int = 8080
            debug: Boolean = false
        }

        devConfig = new Config {
            debug = true
        }

        prodConfig = new Config {
            host = "prod.example.com"
            port = 443
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Imports
// =============================================================================

fn eval_pkl_file(path: &str) -> String {
    let registry = stdlib_registry();
    let evaluator = Evaluator::with_externals(registry);
    match evaluator.eval_file(path) {
        Ok(result) => {
            if let Err(e) = evaluator.force_value(&result) {
                return format!("Force error: {}", e);
            }
            serde_json::to_string_pretty(&result).expect("Failed to serialize")
        }
        Err(e) => format!("Eval error: {}", e),
    }
}

#[test]
fn test_import_basic() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");
    let path = format!("{}/config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_import_aliased() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");
    let path = format!("{}/aliased.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_import_standalone() {
    // Test loading a module with no imports
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures");
    let path = format!("{}/utils.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

// =============================================================================
// Module-level Methods
// =============================================================================

#[test]
fn test_module_level_function() {
    let source = r#"
        function double(x) = x * 2
        function add(a, b) = a + b

        result1 = double(21)
        result2 = add(10, 5)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_module_function_with_module_properties() {
    let source = r#"
        multiplier = 3

        function scale(x) = x * multiplier

        scaled = scale(7)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Type System - is operator
// =============================================================================

#[test]
fn test_is_basic_types() {
    let source = r#"
        int_is_int = 42 is Int
        int_is_string = 42 is String
        string_is_string = "hello" is String
        bool_is_bool = true is Boolean
        null_is_null = null is Null
        float_is_float = 3.14 is Float
        float_is_number = 3.14 is Number
        int_is_number = 42 is Number
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_nullable_types() {
    let source = r#"
        null_is_nullable_string = null is String?
        string_is_nullable_string = "hello" is String?
        int_is_nullable_int = 42 is Int?
        null_is_nullable_int = null is Int?
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_union_types() {
    let source = r#"
        int_is_int_or_string = 42 is Int|String
        string_is_int_or_string = "hello" is Int|String
        bool_is_int_or_string = true is Int|String
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_parameterized_list() {
    let source = r#"
        int_list = new Listing { 1; 2; 3 }
        string_list = new Listing { "a"; "b"; "c" }
        mixed_list = new Listing { 1; "two"; 3 }

        int_list_is_list = int_list.toList() is List
        int_list_is_list_int = int_list.toList() is List<Int>
        int_list_is_list_string = int_list.toList() is List<String>
        string_list_is_list_string = string_list.toList() is List<String>
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_constrained_types() {
    let source = r#"
        positive_num = 42
        negative_num = -10
        zero = 0

        positive_is_positive_int = positive_num is Int(this > 0)
        negative_is_positive_int = negative_num is Int(this > 0)
        zero_is_positive_int = zero is Int(this > 0)
        positive_is_non_negative = positive_num is Int(this >= 0)
        zero_is_non_negative = zero is Int(this >= 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_constrained_string() {
    let source = r#"
        short_str = "hi"
        long_str = "hello world"

        short_is_long = short_str is String(this.length > 5)
        long_is_long = long_str is String(this.length > 5)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_string_literal_type() {
    let source = r#"
        format_json = "json"
        format_yaml = "yaml"

        json_is_json = format_json is "json"
        yaml_is_json = format_yaml is "json"
        json_is_yaml = format_json is "yaml"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_any_type() {
    let source = r#"
        int_is_any = 42 is Any
        string_is_any = "hello" is Any
        null_is_any = null is Any
        list_is_any = (new Listing { 1 }).toList() is Any
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_is_collection_types() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        list = items.toList()

        list_is_list = list is List
        list_is_collection = list is Collection
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Type System - as operator
// =============================================================================

#[test]
fn test_as_compatible_types() {
    let source = r#"
        int_val = 42
        string_val = "hello"

        int_as_int = int_val as Int
        string_as_string = string_val as String
        int_as_any = int_val as Any
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_numeric_coercion() {
    let source = r#"
        int_val = 42
        float_val = 3.14

        int_as_number = int_val as Number
        float_as_number = float_val as Number
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_nullable() {
    let source = r#"
        int_val = 42
        null_val = null

        int_as_nullable_int = int_val as Int?
        null_as_nullable_int = null_val as Int?
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_type_error() {
    let source = r#"
        result = "hello" as Int
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_as_constraint_success() {
    let source = r#"
        positive = 42 as Int(this > 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_constraint_failure() {
    let source = r#"
        result = (-5) as Int(this > 0)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_as_parameterized_list() {
    let source = r#"
        int_list = new Listing { 1; 2; 3 }
        result = int_list.toList() as List<Int>
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_as_parameterized_list_failure() {
    let source = r#"
        int_list = new Listing { 1; 2; 3 }
        result = int_list.toList() as List<String>
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Type System - Combined tests
// =============================================================================

#[test]
fn test_type_check_in_conditional() {
    let source = r#"
        value = 42
        result = if (value is Int) "it's an int" else "not an int"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_type_with_function() {
    let source = r#"
        fn = (x) -> x * 2
        fn_is_function = fn is Function
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Module Inheritance - amends
// =============================================================================

#[test]
fn test_amends_basic() {
    // Test basic module amendment with property inheritance
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/dev_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_amends_nested_objects() {
    // Test that nested objects properly amend parent objects
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/prod_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_amends_chain() {
    // Test chained amendments (A amends B amends C)
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/staging_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_extends_basic() {
    // Test basic module extension (similar to amends but for type extension)
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/inheritance");
    let path = format!("{}/server_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

// =============================================================================
// Amends Scope Tests - Regression tests for scope handling in module amendments
// =============================================================================

#[test]
fn test_amends_inherited_property_refs_child_values() {
    // Test that inherited properties can reference the child's overridden values
    // This was a bug where inherited output.pkg_name = package.name would fail
    // because package was Null (from parent) instead of using child's package
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/amends_scope");
    let path = format!("{}/child_with_ref.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_amends_typed_null_property() {
    // Test that amending a typed null property (e.g., about: About?) creates
    // a properly typed object with class defaults applied
    // This was a bug where a Dynamic object was created instead
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/amends_scope");
    let path = format!("{}/child_with_typed_null.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_class_method_accesses_module_imports() {
    // Test that class methods can access imports/values from their defining module
    // This was a bug where class methods captured the wrong scope
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/amends_scope");
    let path = format!("{}/use_imported_class.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

// =============================================================================
// Comprehensive Tests - Ported from apple/pkl upstream
// These tests are marked #[ignore] until the required features are implemented.
// Run with `cargo test -- --ignored` to check progress.
// =============================================================================

#[test]
fn test_upstream_generators_comprehensive() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/generators_comprehensive.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_string_methods() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/string_methods.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_list_methods() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/list_methods.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_classes_inheritance() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/classes_inheritance.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_type_operators() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/type_operators.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_numeric_methods() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/numeric_methods.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_realworld_config() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/realworld_config.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

#[test]
fn test_upstream_when_generator() {
    let fixtures_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/upstream_tests");
    let path = format!("{}/when_generator.pkl", fixtures_dir);
    insta::assert_snapshot!(eval_pkl_file(&path));
}

// =============================================================================
// Multiline Strings
// =============================================================================

#[test]
fn test_multiline_string() {
    let source = r#"
        text = """
            Hello,
            World!
            """
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_multiline_string_interpolation() {
    let source = r#"
        name = "PKL"
        text = """
            Hello, \(name)!
            Version \(1 + 2)
            """
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Escape Sequences
// =============================================================================

#[test]
fn test_escape_sequences() {
    let source = r#"
        tab = "a\tb"
        newline = "a\nb"
        backslash = "a\\b"
        quote = "a\"b"
        null_char = "a\0b"
        unicode = "\u{41}\u{42}\u{43}"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Numeric Literal Formats
// =============================================================================

#[test]
fn test_hex_literals() {
    let source = r#"
        hex1 = 0xFF
        hex2 = 0x10
        hex3 = 0xDEAD
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_binary_literals() {
    let source = r#"
        bin1 = 0b1010
        bin2 = 0b11111111
        bin3 = 0b0
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_octal_literals() {
    let source = r#"
        oct1 = 0o77
        oct2 = 0o10
        oct3 = 0o755
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_float_exponents() {
    let source = r#"
        e1 = 1e3
        e2 = 2.5e2
        e3 = 1.5e-3
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Pipe Operator
// =============================================================================

#[test]
fn test_pipe_operator() {
    let source = r#"
        double = (x) -> x * 2
        add1 = (x) -> x + 1
        result = 5 |> double
        chained = 5 |> double |> add1
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Non-null Assertion
// =============================================================================

#[test]
fn test_non_null_assertion_success() {
    let source = r#"
        value = "hello"
        result = value!!
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_non_null_assertion_failure() {
    let source = r#"
        value = null
        result = value!!
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Optional Member Access
// =============================================================================

#[test]
fn test_optional_member_access() {
    let source = r#"
        name = "hello"
        len_of_value = name?.length
        len_of_null = null?.length
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Throw Expression
// =============================================================================

#[test]
fn test_throw_expression() {
    let source = r#"
        result = throw("Something went wrong")
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_throw_conditional() {
    let source = r#"
        x = -1
        result = if (x < 0) throw("x must be non-negative") else x
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Boolean Methods
// =============================================================================

#[test]
fn test_boolean_xor() {
    let source = r#"
        tt = true.xor(true)
        tf = true.xor(false)
        ft = false.xor(true)
        ff = false.xor(false)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_boolean_implies() {
    let source = r#"
        tt = true.implies(true)
        tf = true.implies(false)
        ft = false.implies(true)
        ff = false.implies(false)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int Properties (extended)
// =============================================================================

#[test]
fn test_int_properties_extended() {
    let source = r#"
        pos = 42
        neg = -10
        zero = 0

        pos_is_positive = pos.isPositive
        pos_is_negative = pos.isNegative
        pos_is_zero = pos.isZero
        neg_is_positive = neg.isPositive
        neg_is_negative = neg.isNegative
        zero_is_zero = zero.isZero
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Float Properties (extended)
// =============================================================================

#[test]
fn test_float_properties_extended() {
    let source = r#"
        f = 3.14
        is_finite = f.isFinite
        is_infinite = f.isInfinite
        is_nan = f.isNaN
        is_positive = f.isPositive
        is_negative = f.isNegative
        is_zero = f.isZero
        abs_val = (-3.14).abs
        sign_val = (-3.14).sign
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int Methods (extended)
// =============================================================================

#[test]
fn test_int_to_radix_string() {
    let source = r#"
        bin = 255.toRadixString(2)
        oct = 255.toRadixString(8)
        hex = 255.toRadixString(16)
        dec = 255.toRadixString(10)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_int_is_between() {
    let source = r#"
        in_range = 5.isBetween(1, 10)
        at_lower = 1.isBetween(1, 10)
        at_upper = 10.isBetween(1, 10)
        below = 0.isBetween(1, 10)
        above = 11.isBetween(1, 10)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_int_bitwise_operations() {
    let source = r#"
        shl = 1.shl(4)
        shr = 16.shr(2)
        bit_and = 0xFF.and(0x0F)
        bit_or = 0xF0.or(0x0F)
        bit_xor = 0xFF.xor(0x0F)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Duration Arithmetic
// =============================================================================

#[test]
fn test_duration_arithmetic() {
    let source = r#"
        sum = 5.s + 3.s
        diff = 10.s - 4.s
        mixed = 1.min + 30.s
        comparison = 60.s == 1.min
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_duration_properties() {
    let source = r#"
        d = 5.s
        val = d.value
        unit = d.unit
        is_pos = d.isPositive
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// DataSize Arithmetic
// =============================================================================

#[test]
fn test_datasize_arithmetic() {
    let source = r#"
        sum = 512.mb + 512.mb
        comparison = 1024.mb == 1.gb
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_datasize_properties() {
    let source = r#"
        d = 1024.kb
        val = d.value
        unit = d.unit
        is_pos = d.isPositive
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Subscript Access
// =============================================================================

#[test]
fn test_subscript_list() {
    let source = r#"
        items = new Listing { "a"; "b"; "c" }
        list = items.toList()
        first = list[0]
        last = list[2]
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_subscript_map() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        a_val = m["a"]
        c_val = m["c"]
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String Properties
// =============================================================================

#[test]
fn test_string_properties() {
    let source = r#"
        s = "hello"
        empty = ""
        blank = "   "

        s_is_empty = s.isEmpty
        s_is_not_empty = s.isNotEmpty
        s_is_blank = s.isBlank
        s_is_not_blank = s.isNotBlank
        s_last_index = s.lastIndex

        empty_is_empty = empty.isEmpty
        empty_is_blank = empty.isBlank
        blank_is_empty = blank.isEmpty
        blank_is_blank = blank.isBlank
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Additional String Methods
// =============================================================================

#[test]
fn test_string_pad_methods() {
    let source = r#"
        padded_start = "42".padStart(5, "0")
        padded_end = "hi".padEnd(5, ".")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_case_methods() {
    let source = r#"
        cap = "hello".capitalize()
        decap = "Hello".decapitalize()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_take_drop_while() {
    let source = r#"
        taken = "abcdef".take(3)
        dropped = "abcdef".drop(3)
        take_last = "abcdef".takeLast(3)
        drop_last = "abcdef".dropLast(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_index_of() {
    let source = r#"
        idx = "hello world".indexOf("world")
        last_idx = "hello hello".lastIndexOf("hello")
        not_found = "hello".indexOf("xyz")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_string_repeat() {
    let source = r#"
        repeated = "ab".repeat(3)
        empty = "ab".repeat(0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Map Methods and Properties
// =============================================================================

#[test]
fn test_map_methods() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        has_a = m.containsKey("a")
        has_z = m.containsKey("z")
        get_b = m.getOrNull("b")
        get_z = m.getOrNull("z")
        len = m.length
        empty = m.isEmpty
        keys = m.keys
        values = m.values
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Set Type
// =============================================================================

#[test]
fn test_set_creation() {
    let source = r#"
        s = Set(1, 2, 3, 2, 1)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// IntSeq Type
// =============================================================================

#[test]
fn test_intseq() {
    let source = r#"
        seq = IntSeq(0, 5, 1)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Pair Type
// =============================================================================

#[test]
fn test_pair() {
    let source = r#"
        p = Pair("key", 42)
        k = p.first
        v = p.second
        k2 = p.key
        v2 = p.value
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Listing Methods (direct on Listing objects)
// =============================================================================

#[test]
fn test_listing_properties() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        empty = new Listing {}
        len = items.length
        is_empty = items.isEmpty
        empty_is_empty = empty.isEmpty
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_listing_join() {
    let source = r#"
        items = new Listing { "a"; "b"; "c" }
        joined = items.join(", ")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Mapping Methods
// =============================================================================

#[test]
fn test_mapping_methods() {
    let source = r#"
        m = new Mapping {
            ["a"] = 1; ["b"] = 2; ["c"] = 3
        }
        len = m.length
        is_empty = m.isEmpty
        has_a = m.containsKey("a")
        has_z = m.containsKey("z")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Advanced List Methods
// =============================================================================

#[test]
fn test_list_sort() {
    let source = r#"
        nums = new Listing { 3; 1; 4; 1; 5 }
        sorted = nums.toList().sort()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_flat_map() {
    let source = r#"
        nested = new Listing { 1; 2; 3 }
        result = nested.toList().flatMap((x) -> List(x, x * 10))
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_zip() {
    let source = r#"
        a = new Listing { 1; 2; 3 }
        b = new Listing { "a"; "b"; "c" }
        zipped = a.toList().zip(b.toList())
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

#[test]
fn test_list_map_indexed() {
    let source = r#"
        items = new Listing { "a"; "b"; "c" }
        result = items.toList().mapIndexed((i, v) -> "\(i):\(v)")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_filter_indexed() {
    let source = r#"
        items = new Listing { 10; 20; 30; 40; 50 }
        evens = items.toList().filterIndexed((i, v) -> i % 2 == 0)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_fold_indexed() {
    let source = r#"
        items = new Listing { 10; 20; 30 }
        result = items.toList().foldIndexed(0, (i, acc, v) -> acc + i * v)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_find_operations() {
    let source = r#"
        items = new Listing { 1; 2; 3; 4; 5 }
        list = items.toList()
        found = list.findOrNull((x) -> x > 3)
        not_found = list.findOrNull((x) -> x > 10)
        count = list.count((x) -> x % 2 == 0)
        find_index = list.findIndex((x) -> x > 3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_add_replace() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        list = items.toList()
        added = list.add(4)
        replaced = list.replace(1, 99)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_distinct_flatten() {
    let source = r#"
        dup_items = new Listing { 1; 2; 2; 3; 3; 3 }
        distinct = dup_items.toList().distinct()

        nested = List(List(1, 2), List(3, 4))
        flat = nested.flatten()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_min_max() {
    let source = r#"
        items = new Listing { 3; 1; 4; 1; 5; 9 }
        list = items.toList()
        min_val = list.min()
        max_val = list.max()
        min_or_null = list.minOrNull()
        max_or_null = list.maxOrNull()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_single() {
    let source = r#"
        one = List(42)
        single_val = one.single()
        many = List(1, 2)
        single_or_null = many.singleOrNull()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_get_or_null() {
    let source = r#"
        items = List(10, 20, 30)
        valid = items.getOrNull(1)
        invalid = items.getOrNull(99)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_take_drop_last() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        taken = items.take(3)
        dropped = items.drop(2)
        take_last = items.takeLast(2)
        drop_last = items.dropLast(2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_last_index_of() {
    let source = r#"
        items = List(1, 2, 3, 2, 1)
        idx = items.lastIndexOf(2)
        not_found = items.lastIndexOf(99)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_repeat() {
    let source = r#"
        items = List(1, 2)
        repeated = items.repeat(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_list_split() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        result = items.split(3)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Local and Hidden Modifiers
// =============================================================================

#[test]
fn test_local_property() {
    let source = r#"
        local helper = 10
        result = helper * 2
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_hidden_property() {
    let source = r#"
        hidden secret = "classified"
        visible = "public"
        uses_hidden = secret
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Backtick Identifiers
// =============================================================================

#[test]
fn test_backtick_identifier() {
    let source = r#"
        `for` = "value"
        result = `for`
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// This/Outer in Objects
// =============================================================================

#[test]
fn test_this_in_object() {
    let source = r#"
        class Config {
            host: String = "localhost"
            port: Int = 8080
            function url() = "http://\(this.host):\(this.port)"
        }

        config = new Config {}
        url = config.url()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Object Amendment
// =============================================================================

#[test]
fn test_object_amendment() {
    let source = r#"
        class Server {
            host: String = "localhost"
            port: Int = 8080
        }

        base = new Server {}
        amended = (base) { port = 9090 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Spread Operator
// =============================================================================

#[test]
fn test_spread_in_listing() {
    let source = r#"
        first = new Listing { 1; 2 }
        second = new Listing { 3; 4 }
        combined = new Listing {
            ...first
            ...second
        }
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// TypeAlias
// =============================================================================

#[test]
fn test_typealias() {
    let source = r#"
        typealias StringList = List<String>

        items = new Listing { "a"; "b" }
        list = items.toList()
        check = list is StringList
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Abstract Classes
// =============================================================================

#[test]
fn test_abstract_class() {
    let source = r#"
        abstract class Shape {
            name: String
        }

        class Circle extends Shape {
            name = "Circle"
            radius: Float
        }

        c = new Circle { radius = 5.0 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Open Classes
// =============================================================================

#[test]
fn test_open_class() {
    let source = r#"
        open class Base {
            x: Int = 0
        }

        class Child extends Base {
            y: Int = 1
        }

        obj = new Child { x = 10; y = 20 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Class with Methods using module properties
// =============================================================================

#[test]
fn test_class_function_with_params() {
    let source = r#"
        class Calculator {
            function add(a, b) = a + b
            function multiply(a, b) = a * b
        }

        calc = new Calculator {}
        sum = calc.add(3, 4)
        product = calc.multiply(5, 6)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// For Generator in Mapping
// =============================================================================

#[test]
fn test_for_generator_in_mapping() {
    let source = r#"
        names = new Listing { "alice"; "bob"; "carol" }
        nameMap = new Mapping {
            for (name in names.toList()) {
                [name] = name.length
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Nested Object Properties
// =============================================================================

#[test]
fn test_nested_object_access() {
    let source = r#"
        config {
            database {
                host = "localhost"
                port = 5432
            }
            cache {
                ttl = 300.s
            }
        }
        db_host = config.database.host
        cache_ttl = config.cache.ttl
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Complex String Interpolation
// =============================================================================

#[test]
fn test_complex_string_interpolation() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        msg = "Count: \(items.toList().length), First: \(items.toList()[0])"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Empty Collections
// =============================================================================

#[test]
fn test_empty_collections() {
    let source = r#"
        empty_listing = new Listing {}
        empty_mapping = new Mapping {}
        empty_list = List()
        empty_map = Map()
        empty_set = Set()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Chained Method Calls
// =============================================================================

#[test]
fn test_chained_method_calls() {
    let source = r#"
        result = "Hello, World!"
            .toLowerCase()
            .replaceAll(",", "")
            .replaceAll("!", "")
            .split(" ")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int/Float Conversions
// =============================================================================

#[test]
fn test_numeric_conversions() {
    let source = r#"
        int_val = 42
        float_val = 3.14

        int_to_float = int_val.toFloat()
        float_to_int = float_val.toInt()
        int_to_int = int_val.toInt()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// toString Method
// =============================================================================

#[test]
fn test_to_string_method() {
    let source = r#"
        int_str = 42.toString()
        float_str = 3.14.toString()
        bool_str = true.toString()
        null_str = null.toString()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List Properties
// =============================================================================

#[test]
fn test_list_properties() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        len = items.length
        is_empty = items.isEmpty
        is_not_empty = items.isNotEmpty
        first = items.first
        last = items.last
        rest = items.rest
        last_index = items.lastIndex
        is_distinct = items.isDistinct
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Map Properties (inline)
// =============================================================================

#[test]
fn test_map_properties() {
    let source = r#"
        m = Map("x", 1, "y", 2)
        len = m.length
        is_empty = m.isEmpty
        keys = m.keys
        values = m.values
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Float Math Methods
// =============================================================================

#[test]
fn test_float_math_methods() {
    let source = r#"
        sqrt_val = 25.0.sqrt()
        cbrt_val = 27.0.cbrt()
        ceil_val = 2.3.ceil()
        floor_val = 2.7.floor()
        round_val = 2.5.round()
        truncate_val = 2.9.truncate()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String Conversion Methods
// =============================================================================

#[test]
fn test_string_conversion_methods() {
    let source = r#"
        int_from_str = "42".toInt()
        float_from_str = "3.14".toFloat()
        bool_from_str = "true".toBoolean()
        int_or_null = "abc".toIntOrNull()
        float_or_null = "xyz".toFloatOrNull()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Multiple Class Hierarchy
// =============================================================================

#[test]
fn test_deep_class_hierarchy() {
    let source = r#"
        class A {
            x: Int = 1
        }
        class B extends A {
            y: Int = 2
        }
        class C extends B {
            z: Int = 3
        }

        obj = new C { x = 10 }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Class with Default Values and Overrides
// =============================================================================

#[test]
fn test_class_defaults_and_overrides() {
    let source = r#"
        class Endpoint {
            method: String = "GET"
            path: String
            timeout: Duration = 30.s
        }

        health = new Endpoint {
            path = "/health"
        }

        slow = new Endpoint {
            method = "POST"
            path = "/upload"
            timeout = 120.s
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Listing with Typed Elements
// =============================================================================

#[test]
fn test_listing_with_strings() {
    let source = r#"
        class Tag {
            name: String
            color: String = "gray"
        }

        tags = new Listing {
            new Tag { name = "urgent"; color = "red" }
            new Tag { name = "low" }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Mapping with Complex Keys/Values
// =============================================================================

#[test]
fn test_mapping_with_computed_keys() {
    let source = r#"
        prefix = "key"
        m = new Mapping {
            ["\(prefix)_1"] = 10
            ["\(prefix)_2"] = 20
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// When Generator with Else
// =============================================================================

#[test]
fn test_when_else_generator() {
    let source = r#"
        isDev = false
        config = new {
            name = "app"
            when (isDev) {
                logLevel = "debug"
                verboseErrors = true
            } else {
                logLevel = "warn"
                verboseErrors = false
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Multiple When Generators
// =============================================================================

#[test]
fn test_multiple_when_generators() {
    let source = r#"
        enableCache = true
        enableLogging = false

        config = new {
            name = "service"
            when (enableCache) {
                cacheSize = 1024
            }
            when (enableLogging) {
                logFile = "/var/log/app.log"
            }
        }
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Negation of Expressions
// =============================================================================

#[test]
fn test_unary_negation() {
    let source = r#"
        a = 42
        neg_a = -a
        neg_lit = -10
        double_neg = -(-5)
        not_true = !true
        not_expr = !(1 > 2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Complex Conditional Chains
// =============================================================================

#[test]
fn test_nested_conditionals() {
    let source = r#"
        x = 15
        result = if (x > 20) "high" else if (x > 10) "medium" else "low"
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Let with Complex Expressions
// =============================================================================

#[test]
fn test_let_complex() {
    let source = r#"
        items = new Listing { 1; 2; 3; 4; 5 }
        result = let (list = items.toList()) let (sum = list.fold(0, (a, b) -> a + b)) sum
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Int compareTo
// =============================================================================

#[test]
fn test_compare_to() {
    let source = r#"
        less = 1.compareTo(2)
        equal = 5.compareTo(5)
        greater = 10.compareTo(3)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List sortBy
// =============================================================================

#[test]
fn test_list_sort_by() {
    let source = r#"
        items = List("banana", "apple", "cherry")
        sorted = items.sortBy((s) -> s.length)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List groupBy
// =============================================================================

#[test]
fn test_list_group_by() {
    let source = r#"
        items = List(1, 2, 3, 4, 5, 6)
        grouped = items.groupBy((x) -> if (x % 2 == 0) "even" else "odd")
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// List partition
// =============================================================================

#[test]
fn test_list_partition() {
    let source = r#"
        items = List(1, 2, 3, 4, 5)
        result = items.partition((x) -> x > 3)
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Map remove
// =============================================================================

#[test]
fn test_map_remove() {
    let source = r#"
        m = Map("a", 1, "b", 2, "c", 3)
        removed = m.remove("b")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List to Map
// =============================================================================

#[test]
fn test_list_to_map() {
    let source = r#"
        items = List("alice", "bob", "carol")
        m = items.toMap((name) -> name, (name) -> name.length)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// List to Set
// =============================================================================

#[test]
fn test_list_to_set() {
    let source = r#"
        items = List(1, 2, 2, 3, 3, 3)
        s = items.toSet()
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Map entries
// =============================================================================

#[test]
fn test_map_entries() {
    let source = r#"
        m = Map("x", 1, "y", 2)
        entries = m.entries
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Annotations
// =============================================================================

#[test]
fn test_annotation_on_class() {
    let source = r#"
        class Deprecated {
            message: String = ""
        }

        @Deprecated { message = "Use NewConfig" }
        class OldConfig {
            value: Int = 0
        }

        obj = new OldConfig { value = 42 }
    "#;
    insta::assert_snapshot!(eval_pkl_result(source));
}

// =============================================================================
// Listing map method
// =============================================================================

#[test]
fn test_listing_map_method() {
    let source = r#"
        items = new Listing { 1; 2; 3 }
        doubled = items.map((x) -> x * 2)
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// String codePoints property
// =============================================================================

#[test]
fn test_string_codepoints() {
    let source = r#"
        s = "ABC"
        codepoints = s.codePoints
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Duration/DataSize toUnit
// =============================================================================

#[test]
fn test_duration_to_unit() {
    let source = r#"
        d = 2.h
        in_minutes = d.toUnit("min")
        in_seconds = d.toUnit("s")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

#[test]
fn test_datasize_to_unit() {
    let source = r#"
        d = 1.gb
        in_mb = d.toUnit("mb")
        in_kb = d.toUnit("kb")
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}

// =============================================================================
// Mapping toMap conversion
// =============================================================================

#[test]
fn test_mapping_to_map() {
    let source = r#"
        m = new Mapping {
            ["a"] = 1; ["b"] = 2
        }
        converted = m.toMap()
        keys = converted.keys
    "#;
    insta::assert_snapshot!(eval_pkl(source));
}
